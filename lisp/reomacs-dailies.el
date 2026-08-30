;;; reomacs-dailies.el --- Work dailies helpers -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (dash "2.20.0"))
;; Keywords: convenience, outlines

;;; Commentary:

;; Maintain flat Org work dailies, fetch forge interactions, display logged
;; hours, and copy daily entries as Markdown.

;;; Code:

(require 'calendar)
(require 'dash)
(require 'json)
(require 'org)
(require 'seq)
(require 'subr-x)

(defcustom reomacs/dailies-file "~/Projects/Baringa/dailies.org"
  "Path to the Org file used for work dailies."
  :type 'file
  :group 'reo101)

(defun reomacs/dailies--buffer-p (&optional buffer)
  "Return non-nil when BUFFER visits `reomacs/dailies-file'."
  (when-let* ((buffer (or buffer (current-buffer)))
              (file (buffer-file-name buffer)))
    (file-equal-p file (expand-file-name reomacs/dailies-file))))

(defun reomacs/dailies--buffer ()
  "Return the buffer visiting `reomacs/dailies-file', opening it if needed."
  (let ((file (expand-file-name reomacs/dailies-file)))
    (or (get-file-buffer file)
        (find-file-noselect file))))

(defcustom reomacs/dailies-daily-target-hours nil
  "Daily hours target, or nil to disable day/week status displays.
The weekly target is five times this value."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'reo101
  :safe (lambda (value) (and (numberp value) (> value 0))))

(defun reomacs/dailies--week-id (time)
  "Return ISO week identifier for TIME, such as \"2026-W35\"."
  (format-time-string "%G-W%V" time))

(defconst reomacs/dailies--date-heading-title-re
  "\\`<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
  "Matches a date at the start of a metadata-free Org heading title.")

(defun reomacs/dailies--date-at-heading ()
  "Return the ISO date at the current level-two Org heading, or nil.
TODO state, priority, tags and COMMENT metadata are ignored."
  (when (and (org-at-heading-p) (= (org-current-level) 2))
    (let ((title (org-get-heading t t t t)))
      (when (string-match reomacs/dailies--date-heading-title-re title)
        (match-string-no-properties 1 title)))))

(defun reomacs/dailies--goto-containing-date-heading ()
  "Move to the containing date heading and return its ISO date.
Return nil when point is not inside a level-two date subtree."
  (let ((found (or (org-at-heading-p)
                   (ignore-errors (org-back-to-heading t) t))))
    (while (and found (> (org-current-level) 2))
      (setq found (org-up-heading-safe)))
    (and found (reomacs/dailies--date-at-heading))))

;;;###autoload
(defun reomacs/dailies-new-day ()
  "Create today's flat dailies entry, or the next workday if it exists."
  (interactive)
  (let* ((dailies-buf (reomacs/dailies--buffer))
         (today (calendar-current-date))
         (today-time (encode-time 0 0 0
                                  (nth 1 today) (nth 0 today) (nth 2 today)))
         last-date-pos
         last-date-str)
    (with-current-buffer dailies-buf
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (goto-char (match-beginning 0))
        (when-let* ((date (reomacs/dailies--date-at-heading)))
          (setq last-date-pos (point)
                last-date-str date))
        (forward-line 1))
      (unless last-date-pos
        (user-error "No date entries found in dailies file"))
      (let* ((parts (mapcar #'string-to-number
                            (split-string last-date-str "-")))
             (last-time (encode-time 0 0 0
                                     (nth 2 parts) (nth 1 parts) (nth 0 parts)))
             (new-time (if (time-less-p last-time today-time)
                           today-time
                         (reomacs/dailies--next-workday last-time)))
             (new-date-str (format-time-string "<%Y-%m-%d %a>" new-time))
             (last-week-id (reomacs/dailies--week-id last-time))
             (new-week-id (reomacs/dailies--week-id new-time))
             (insert-pos (save-excursion
                           (goto-char last-date-pos)
                           (org-end-of-subtree t)
                           (point))))
        (goto-char insert-pos)
        (skip-chars-backward " \t\n")
        (end-of-line)
        (insert "\n\n")
        (unless (equal last-week-id new-week-id)
          (insert "* Week " new-week-id "\n\n"))
        (insert "** " (or (car org-not-done-keywords) "TODO")
                " " new-date-str "\n\n+ ")
        (reomacs/dailies-refresh-week-status)
        (switch-to-buffer dailies-buf)
        (message "Created new daily entry for %s" new-date-str)))))

(defun reomacs/dailies--next-workday (from-date)
  "Return the next workday after FROM-DATE (a time value).
Skips weekends. Could be extended for holidays."
  (-let* ((one-day (* 24 60 60))
          (next-time (time-add from-date one-day))
          (dow (string-to-number (format-time-string "%u" next-time)))) ; 1=Mon, 7=Sun
    ;; Skip weekends
    (while (>= dow 6)
      (setq next-time (time-add next-time one-day))
      (setq dow (string-to-number (format-time-string "%u" next-time))))
    next-time))


(defun reomacs/dailies--string-alist-p (value)
  "Return non-nil when VALUE is an alist with string keys and values."
  (and (proper-list-p value)
       (seq-every-p (lambda (entry)
                      (and (consp entry)
                           (stringp (car entry))
                           (stringp (cdr entry))))
                    value)))

(defcustom reomacs/dailies-repo-aliases '()
  "Alist mapping repo identities to display names for dailies.
Keys may be written as \"owner/repo\" (GitHub), \"org/project/repo\"
(Azure DevOps), or any URL/remote containing them — both keys and
lookups go through `reomacs/dailies--canonical-repo-id', so all shapes
resolve against each other.  Set as a file-local variable in
dailies.org."
  :type '(alist :key-type string :value-type string)
  :group 'reo101
  :safe #'reomacs/dailies--string-alist-p)

(defcustom reomacs/dailies-forge 'github
  "Git forge the interactions fetchers query.
Set as a file-local variable in dailies.org to vary per workspace."
  :type '(choice (const :tag "GitHub (gh CLI)" github)
                 (const :tag "Azure DevOps (az CLI)" azure))
  :group 'reo101
  :safe (lambda (value) (memq value '(github azure))))

(defcustom reomacs/dailies-azure-org "https://dev.azure.com/baringa"
  "Azure DevOps organization URL, used when `reomacs/dailies-forge' is azure.
Set as a file-local variable in dailies.org if you deal with several orgs."
  :type 'string
  :group 'reo101
  :safe #'stringp)

(defun reomacs/dailies--format-event (event-type props)
  "Format an Org line for EVENT-TYPE using PROPS."
  (let ((url (plist-get props :url))
        (name (plist-get props :display-name))
        (number (plist-get props :number))
        (branch (plist-get props :branch))
        (actor (plist-get props :actor)))
    (pcase event-type
      ('pr-create
       (format "+ Create [[%s][~%s~ PR %s]]" url name number))
      ((or 'pr-close 'pr-merge)
       (format "+ %s [[%s][~%s~ PR %s]]%s"
               (if (eq event-type 'pr-merge) "Merge" "Close")
               url name number
               (if actor (format " (by @%s)" actor) "")))
      ('pr-review
       (format "+ Review [[%s][~%s~ PR %s]]" url name number))
      ('issue-create
       (format "+ Create [[%s][~%s~ Issue %s]]" url name number))
      ('push-with-pr
       (format "+ Push to ~%s~ branch ~%s~ ([[%s][PR %s]])"
               name branch url number))
      ('push-no-pr
       (format "+ Push to ~%s~ branch ~%s~" name branch))
      (_ (error "Unknown dailies event: %s" event-type)))))

(defun reomacs/dailies--canonical-repo-id (repo-id)
  "Canonicalize URL, SSH remote, or bare REPO-ID to its forge path."
  (let* ((value (replace-regexp-in-string "[?#].*\\'" "" (string-trim repo-id)))
         (path
          (cond
           ((string-match
             "\\`[[:alpha:]][[:alnum:]+.-]*://[^/]+/\\(.*\\)\\'" value)
            (match-string 1 value))
           ((string-match "\\`[^@/]+@[^:]+:\\(.*\\)\\'" value)
            (match-string 1 value))
           ((string-match
             "\\`\\(?:github\\.com\\|dev\\.azure\\.com\\)/\\(.*\\)\\'"
             value)
            (match-string 1 value))
           (t value)))
         (path (replace-regexp-in-string "\\.git\\'" "" path))
         (parts (split-string path "/" t)))
    (string-join (seq-remove (lambda (part) (string= part "_git")) parts)
                 "/")))

(defun reomacs/dailies--repo-display-name (aliases repo-id repo-name)
  "Get display name for REPO-ID from ALIASES, falling back to REPO-NAME.
Both sides pass through `reomacs/dailies--canonical-repo-id' before
comparison.  ALIASES is taken from the runner plist — captured in the
dailies buffer at command time, because results format asynchronously
outside of it where file-local values are not visible."
  (-if-let (match (--first (string= (reomacs/dailies--canonical-repo-id repo-id)
                                    (reomacs/dailies--canonical-repo-id (car it)))
                           aliases))
      (cdr match)
    repo-name))

(defun reomacs/dailies--date-on-day-p (iso-date target-date)
  "Return non-nil when ISO-DATE falls on TARGET-DATE.
ISO-DATE is a timestamp; TARGET-DATE has the form YYYY-MM-DD."
  (and iso-date
       (not (string-prefix-p "0001-" iso-date))
       (string-prefix-p target-date iso-date)))

(defvar reomacs/dailies--username-commands
  '((github . ("gh" "api" "user" "--jq" ".login"))
    (azure . ("az" "account" "show" "--query" "user.name" "--output" "tsv")))
  "Alist mapping forge symbols to argv fetching the current user's login.")

(defun reomacs/dailies--username-command ()
  "argv fetching the username for `reomacs/dailies-forge'."
  (or (alist-get reomacs/dailies-forge reomacs/dailies--username-commands)
      (user-error "Unknown dailies forge: %s" reomacs/dailies-forge)))


;; A runner captures asynchronous fetch state and buffer-local settings.

(defun reomacs/dailies--run-gh-fetch (runner)
  "Execute GitHub interactions fetch using RUNNER abstraction."
  (-let* (((&plist :date :username :fetch :spawn :finish) runner)
          (next-date (--> (concat date "T00:00:00")
                          (date-to-time it)
                          (time-add it (* 24 60 60))
                          (format-time-string "%Y-%m-%d" it)))
          (date-filter (format "--updated=%s..%s" date next-date)))
    ;; Spawn 4 parallel searches
    (-dotimes 4 (lambda (_) (funcall spawn)))
    ;; Authored PRs
    (funcall fetch
             (format "gh search prs --author=@me %s --json number,repository,url,createdAt,closedAt,state" date-filter)
             (lambda (output)
               (-some--> output
                 (ignore-errors (json-parse-string it :array-type 'list :object-type 'alist))
                 (reomacs/dailies--process-prs runner it))
               (funcall finish)))
    ;; Reviewed PRs
    (funcall fetch
             (format "gh search prs --reviewed-by=@me %s --json number,repository,url" date-filter)
             (lambda (output)
               (-some--> output
                 (ignore-errors (json-parse-string it :array-type 'list :object-type 'alist))
                 (reomacs/dailies--process-reviews runner it))
               (funcall finish)))
    ;; Authored issues
    (funcall fetch
             (format "gh search issues --author=@me %s --json number,repository,url,createdAt" date-filter)
             (lambda (output)
               (-some--> output
                 (ignore-errors (json-parse-string it :array-type 'list :object-type 'alist))
                 (reomacs/dailies--process-issues runner it))
               (funcall finish)))
    ;; Branch pushes
    (funcall fetch
             (list "gh" "api" (format "/users/%s/events?per_page=100" username)
                   "--jq" (format "[.[] | select(.type == \"PushEvent\" and (.created_at | startswith(\"%s\")))] | unique_by(.repo.name + .payload.ref) | .[] | {repo: .repo.name, branch: .payload.ref}"
                                  date))
             (lambda (output)
               (-some--> (string-trim output)
                 (unless (string-empty-p it) it)
                 (split-string it "\n" t)
                 (reomacs/dailies--process-pushes runner it))
               (funcall finish)))))

(defun reomacs/dailies--process-prs (runner prs)
  "Process authored PRs using RUNNER."
  (-let [(&plist :date :username :aliases :fetch :emit :spawn :finish) runner]
    (--each prs
      (-let* [((&alist 'url 'number 'createdAt 'closedAt
                       'repository (&alist 'name 'nameWithOwner)) it)
              (display-name (reomacs/dailies--repo-display-name aliases nameWithOwner name))
              (props (list :url url :display-name display-name :number number))]
        (cond
         ((reomacs/dailies--date-on-day-p createdAt date)
          (funcall emit (reomacs/dailies--format-event 'pr-create props)))
         ((reomacs/dailies--date-on-day-p closedAt date)
          (funcall spawn)
          (funcall fetch
                   (list "gh" "api" (format "/repos/%s/issues/%s/events" nameWithOwner number)
                         "--jq" "[.[] | select(.event == \"closed\" or .event == \"merged\")] | last | {actor: .actor.login, event: .event}")
                   (lambda (output)
                     (-let* [((&alist 'actor 'event) (ignore-errors (json-parse-string (string-trim output) :object-type 'alist)))
                             (event-sym (if (string= event "merged") 'pr-merge 'pr-close))
                             (props-final (-if-let (other (and (not (string= actor username)) actor))
                                              (plist-put (copy-sequence props) :actor other)
                                            props))]
                       (funcall emit (reomacs/dailies--format-event event-sym props-final)))
                     (funcall finish)))))))))

(defun reomacs/dailies--process-reviews (runner prs)
  "Process reviewed PRs using RUNNER."
  (-let [(&plist :date :username :aliases :fetch :emit :spawn :finish) runner]
    (--each prs
      (-let* [((&alist 'url 'number 'repository (&alist 'name 'nameWithOwner)) it)
              (display-name (reomacs/dailies--repo-display-name aliases nameWithOwner name))]
        (funcall spawn)
        (funcall fetch
                 (list "gh" "api" (format "/repos/%s/pulls/%s/reviews" nameWithOwner number)
                       "--jq" (format "[.[] | select(.user.login == \"%s\" and (.submitted_at | startswith(\"%s\")))] | length"
                                      username date))
                 (lambda (output)
                   (-let [count (-> output string-trim string-to-number)]
                     (when (> count 0)
                       (funcall emit (reomacs/dailies--format-event
                                      'pr-review (list :url url :display-name display-name :number number)))))
                   (funcall finish)))))))

(defun reomacs/dailies--process-issues (runner issues)
  "Process authored issues using RUNNER."
  (-let [(&plist :date :aliases :emit) runner]
    (--each issues
      (-let* [((&alist 'url 'number 'createdAt 'repository (&alist 'name 'nameWithOwner)) it)
              (display-name (reomacs/dailies--repo-display-name aliases nameWithOwner name))]
        (when (reomacs/dailies--date-on-day-p createdAt date)
          (funcall emit (reomacs/dailies--format-event
                         'issue-create (list :url url :display-name display-name :number number))))))))

(defun reomacs/dailies--process-pushes (runner lines)
  "Process branch push events using RUNNER."
  (-let [(&plist :aliases :fetch :emit :spawn :finish) runner]
    (--each lines
      (-when-let* [((&alist 'repo 'branch) (ignore-errors (json-parse-string it :object-type 'alist)))
                   (branch-name (replace-regexp-in-string "^refs/heads/" "" branch))
                   (repo-name (-last-item (split-string repo "/")))
                   (display-name (reomacs/dailies--repo-display-name aliases repo repo-name))
                   (owner (-first-item (split-string repo "/")))]
        (funcall spawn)
        (funcall fetch
                 (list "gh" "api" (format "/repos/%s/pulls?head=%s:%s&state=all" repo owner branch-name)
                       "--jq" ".[0] | {number, html_url}")
                 (lambda (output)
                   (-let [(&alist 'number 'html_url) (ignore-errors (json-parse-string (string-trim output) :object-type 'alist))]
                     (funcall emit (if number
                                       (reomacs/dailies--format-event
                                        'push-with-pr (list :display-name display-name :branch branch-name
                                                            :url html_url :number number))
                                     (reomacs/dailies--format-event
                                      'push-no-pr (list :display-name display-name :branch branch-name)))))
                   (funcall finish)))))))

;; ----------------------------------------------------------------------
;; Azure DevOps (az CLI) fetcher
;; ----------------------------------------------------------------------

(defun reomacs/dailies--az-org-name (org)
  "Return the organization segment of Azure DevOps URL ORG."
  (car (last (split-string (string-trim-right org "/") "/" t))))

(defun reomacs/dailies--az-pr-browser-url (org pr)
  "Browser-facing URL for Azure DevOps PR alist PR in ORG."
  (-let* (((&alist 'pullRequestId number
                   'repository (&alist 'name repo-name
                                       'project (&alist 'name project)))
           pr))
    (format "%s/%s/_git/%s/pullrequest/%s"
            (string-trim-right org "/") project repo-name number)))

(defun reomacs/dailies--az-repo-id (org pr)
  "Canonical \"org/project/repo\" identity for Azure DevOps PR alist PR in ORG.
Matches alias keys written either as that path or as a full URL."
  (-let* (((&alist 'repository (&alist 'name repo-name
                                       'project (&alist 'name project)))
           pr))
    (string-join (list (reomacs/dailies--az-org-name org) project repo-name) "/")))

(defun reomacs/dailies--az-threads-cmd (org pr)
  "argv fetching discussion threads for Azure DevOps PR alist PR in ORG."
  (-let* (((&alist 'pullRequestId number
                   'repository (&alist 'id repo-id
                                       'project (&alist 'name project)))
           pr))
    (list "az" "devops" "invoke"
          "--org" org
          "--area" "git"
          "--resource" "pullRequestThreads"
          "--route-parameters"
          (format "project=%s" project)
          (format "repositoryId=%s" repo-id)
          (format "pullRequestId=%s" number)
          "-o" "json")))

(defun reomacs/dailies--az-review-candidate-p (pr username date)
  "Cheap pre-filter: might Azure PR hold my review activity around DATE?
Pays for a per-PR threads request only when I am an assigned reviewer,
or the PR was created or closed on DATE and is not mine."
  (let ((creator (alist-get 'uniqueName (alist-get 'createdBy pr))))
    (and (not (string= username creator))
         (or (member username (--map (alist-get 'uniqueName it)
                                     (alist-get 'reviewers pr)))
             (reomacs/dailies--date-on-day-p (alist-get 'creationDate pr) date)
             (reomacs/dailies--date-on-day-p (alist-get 'closedDate pr) date)))))

(defun reomacs/dailies--process-az-prs (runner prs)
  "Emit events for Azure DevOps PR alists PRS using RUNNER."
  (-let* (((&plist :date :aliases :azure-org :username
                   :emit :spawn :fetch :finish)
           runner))
    (--each prs
      (-let* ((display-name (reomacs/dailies--repo-display-name
                             aliases
                             (reomacs/dailies--az-repo-id azure-org it)
                             (alist-get 'name (alist-get 'repository it))))
              (props (list :url (reomacs/dailies--az-pr-browser-url azure-org it)
                           :display-name display-name
                           :number (alist-get 'pullRequestId it)))
              (status (alist-get 'status it))
              ;; Unlike gh search --author=@me, the project listing returns
              ;; everyone's PRs, so gate lifecycle events on creator.
              (mine (string= username
                             (alist-get 'uniqueName (alist-get 'createdBy it)))))
        (cond
         ((and mine
               (reomacs/dailies--date-on-day-p (alist-get 'creationDate it) date))
          (funcall emit (reomacs/dailies--format-event 'pr-create props)))
         ((and mine
               (member status '("completed" "abandoned"))
               (reomacs/dailies--date-on-day-p (alist-get 'closedDate it) date))
          ;; Azure has no separate merge/close distinction beyond status:
          ;; completed means the PR was completed (merged), abandoned not.
          (funcall emit (reomacs/dailies--format-event
                         (if (string= status "completed") 'pr-merge 'pr-close)
                         props))))
        ;; Review scan: did I comment on any thread on the target day?
        (when (reomacs/dailies--az-review-candidate-p it username date)
          (funcall spawn)
          (funcall fetch
                   (reomacs/dailies--az-threads-cmd azure-org it)
                   (lambda (output)
                     (-some--> output
                       (ignore-errors
                         (json-parse-string (string-trim it)
                                            :array-type 'list :object-type 'alist
                                            :null-object nil :false-object nil))
                       (alist-get 'value it)
                       (reomacs/dailies--process-az-threads runner props it))
                     (funcall finish))))))))

(defun reomacs/dailies--process-az-threads (runner props threads)
  "Emit a review event if THREADS show my comments on RUNNER's date."
  (-let [(&plist :date :username :emit) runner]
    ;; Bind `thread' explicitly: a nested --any? would shadow the outer `it'.
    (when (--any? (let ((thread it))
                    (--any? (and (reomacs/dailies--date-on-day-p
                                  (alist-get 'publishedDate it) date)
                                 (string= (alist-get 'uniqueName (alist-get 'author it))
                                          username)
                                 (not (equal (alist-get 'commentType it) "system")))
                            (alist-get 'comments thread)))
                  threads)
      (funcall emit (reomacs/dailies--format-event 'pr-review props)))))

(defun reomacs/dailies--run-azure-fetch (runner)
  "Execute Azure DevOps interactions fetch using RUNNER abstraction.
Azure's org-wide PR listing only ever returns PRs created by the
caller, so enumerate projects first, then scan each project's PRs.
Pushes have no queryable feed in Azure DevOps and are not fetched."
  (-let* (((&plist :azure-org :fetch :spawn :finish) runner))
    (funcall spawn)
    (funcall fetch
             (list "az" "devops" "project" "list" "--org" azure-org "-o" "json")
             (lambda (output)
               (-when-let* ((projects (ignore-errors
                                        (json-parse-string (string-trim output)
                                                           :array-type 'list
                                                           :object-type 'alist
                                                           :null-object nil
                                                           :false-object nil)))
                            (values (alist-get 'value projects)))
                 (--each values
                   (funcall spawn)
                   (funcall fetch
                            (list "az" "repos" "pr" "list" "--org" azure-org
                                  "--project" (alist-get 'name it)
                                  "--status" "all" "--top" "500" "-o" "json")
                            (lambda (prs-output)
                              (when-let* ((prs
                                          (ignore-errors
                                            (json-parse-string
                                             (string-trim prs-output)
                                             :array-type 'list
                                             :object-type 'alist
                                             :null-object nil
                                             :false-object nil))))
                                (reomacs/dailies--process-az-prs runner prs))
                              (funcall finish))))
               (funcall finish))))))

(defvar reomacs/dailies--forge-fetchers
  '((github . reomacs/dailies--run-gh-fetch)
    (azure . reomacs/dailies--run-azure-fetch))
  "Alist mapping `reomacs/dailies-forge' symbols to fetch functions.
Each function receives the runner plist.")

(defun reomacs/dailies--run-forge-fetch (runner)
  "Dispatch RUNNER to the fetcher for its :forge."
  (-if-let (fetcher (alist-get (plist-get runner :forge)
                               reomacs/dailies--forge-fetchers))
      (funcall fetcher runner)
    (user-error "Unknown dailies forge: %s" (plist-get runner :forge))))

(defun reomacs/dailies--gh-async (command callback)
  "Run gh COMMAND asynchronously, call CALLBACK with output string.
COMMAND can be a string (split by shell rules) or a list of args.
Sets TERM=dumb and sends EOF to prevent gh from blocking on terminal."
  (-let* ((process-environment (cons "TERM=dumb" process-environment))
          (buf (generate-new-buffer " *gh-async*"))
          (cmd-list (if (listp command) command
                      (split-string-shell-command command)))
          (proc (make-process
                 :name "gh-async"
                 :buffer buf
                 :connection-type 'pipe
                 :command cmd-list
                 :sentinel
                 (lambda (proc event)
                   (cond
                    ((string-match-p "finished" event)
                     (unwind-protect
                         (funcall callback (with-current-buffer (process-buffer proc)
                                             (buffer-string)))
                       (kill-buffer (process-buffer proc))))
                    ((string-match-p "\\(exited\\|failed\\)" event)
                     (ignore-errors (kill-buffer (process-buffer proc)))))))))
    (process-send-eof proc)
    proc))

(defconst reomacs/dailies--placeholder "⏳ Fetching forge interactions..."
  "Placeholder text shown while async fetch is in progress.")


(defun reomacs/dailies--make-async-runner (date marker)
  "Create an async runner for DATE with MARKER for insertion.
Returns a plist with runner functions using async state management.
Buffer-local settings are captured here while the command still runs
inside the dailies buffer."
  (-let* ((state (make-hash-table :test 'eq))
          (forge reomacs/dailies-forge))
    (puthash :pending 0 state)
    (puthash :marker marker state)
    (list :date date
          :forge reomacs/dailies-forge
          :aliases reomacs/dailies-repo-aliases
          :azure-org reomacs/dailies-azure-org
          :username nil  ; filled in by initial fetch
          :state state
          :fetch (lambda (cmd callback)
                   (reomacs/dailies--gh-async cmd callback))
          :emit (lambda (line)
                  (-when-let (m (gethash :marker state))
                    (when (marker-buffer m)
                      (with-current-buffer (marker-buffer m)
                        (save-excursion
                          (goto-char m)
                          (insert-before-markers line "\n"))))))
          :spawn (lambda ()
                   (puthash :pending (1+ (gethash :pending state)) state))
          :finish (lambda ()
                    (puthash :pending (1- (gethash :pending state)) state)
                    (when (<= (gethash :pending state) 0)
                      (-when-let (m (gethash :marker state))
                        (when (marker-buffer m)
                          (with-current-buffer (marker-buffer m)
                            (save-excursion
                              (goto-char m)
                              (when (looking-at (regexp-quote (concat "+ " reomacs/dailies--placeholder)))
                                (delete-region (point) (line-beginning-position 2))))))
                        (message "%s interactions fetch complete"
                                 forge)
                        (set-marker m nil)))))))

(defun reomacs/dailies--fetch-interactions-async (date marker)
  "Fetch forge interactions for DATE asynchronously.
MARKER points to placeholder line; results insert above it."
  (-let [runner (reomacs/dailies--make-async-runner date marker)]
    ;; First get username, then run the fetch
    (reomacs/dailies--gh-async
     (reomacs/dailies--username-command)
     (lambda (output)
       (plist-put runner :username (string-trim output))
       (reomacs/dailies--run-forge-fetch runner)))))

(defun reomacs/dailies--previous-workday (from-time)
  "Return the previous workday before FROM-TIME.
Skips weekends going backwards."
  (-let* ((one-day (* 24 60 60))
          (prev-time (time-subtract from-time one-day))
          (dow (string-to-number (format-time-string "%u" prev-time))))
    (while (>= dow 6)
      (setq prev-time (time-subtract prev-time one-day))
      (setq dow (string-to-number (format-time-string "%u" prev-time))))
    prev-time))

(defun reomacs/dailies--heading-date ()
  "Return the containing date heading as an Emacs time value, or nil."
  (save-excursion
    (-when-let (date (reomacs/dailies--goto-containing-date-heading))
      (date-to-time (concat date "T00:00:00")))))

;;;###autoload
(defun reomacs/dailies-insert-interactions (days-back)
  "Insert git forge interactions asynchronously.
Queries the forge configured by `reomacs/dailies-forge' and replaces a
placeholder as results arrive.  DAYS-BACK nil uses the previous
workday, while zero uses today."
  (interactive
   (list (let ((input (read-string "Days back (empty for smart yesterday, 0 for today): ")))
           (if (string-empty-p input) nil (string-to-number input)))))
  (-let* ((target-date
           (cond
            ((null days-back)
             (-if-let (heading-time (reomacs/dailies--heading-date))
                 (format-time-string "%Y-%m-%d"
                                     (reomacs/dailies--previous-workday heading-time))
               (user-error "No date heading found above point")))
            (t
             (format-time-string "%Y-%m-%d"
                                 (time-subtract (current-time)
                                                (* days-back 24 60 60))))))
          (marker (point-marker)))
    (insert "+ " reomacs/dailies--placeholder "\n")
    (set-marker marker (line-beginning-position 0))  ; point to placeholder line
    (set-marker-insertion-type marker t)  ; marker stays put when inserting before it
    (message "Fetching %s interactions for %s..." reomacs/dailies-forge target-date)
    (reomacs/dailies--fetch-interactions-async target-date marker)))

;; ----------------------------------------------------------------------
;; Duration annotations

(defconst reomacs/dailies--duration-re
  (concat "{\\s-*"
          "\\(?:\\([0-9]+\\(?:\\.[0-9]+\\)?\\)h\\)?\\s-*"
          "\\(?:\\([0-9]+\\)m\\)?\\s-*"
          "\\(?:@\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\)?\\s-*"
          "\\(✓\\)?}")
  "Match a dailies duration, with capture groups for hours and minutes.")

(defconst reomacs/dailies--trailing-duration-re
  (concat "[ \\t]+" reomacs/dailies--duration-re "[ \\t]*\\'")
  "Match a dailies duration annotation at the end of a line.")

(defun reomacs/dailies--parse-duration (hours minutes)
  "Convert HOURS/MINUTES match groups (strings or nil) to whole minutes."
  (truncate (+ (* (string-to-number (or hours "0")) 60)
               (string-to-number (or minutes "0")))))

(defun reomacs/dailies--sum-duration-minutes (beg end)
  "Sum trailing duration annotations on bullets between BEG and END.
Submitted markers still count toward logged work."
  (save-excursion
    (goto-char beg)
    (let ((total 0))
      (while (< (point) end)
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (and (string-match "^[ \t]*\\+[ \t]+" line)
                     (string-match reomacs/dailies--trailing-duration-re line))
            (setq total
                  (+ total
                     (reomacs/dailies--parse-duration
                      (match-string 1 line) (match-string 2 line))))))
        (forward-line 1))
      total)))

(defun reomacs/dailies--format-hours (minutes)
  "Format MINUTES as compact decimal hours."
  (format "%g" (/ minutes 60.0)))

(defface reomacs/dailies-hours-target-face
  '((t (:inherit shadow :height 0.5)))
  "Face for the target portion of dynamic hours status displays."
  :group 'reo101)

(defvar-local reomacs/dailies--week-status-overlays nil
  "Dynamic week/day status overlays in the current dailies buffer.")

(defvar-local reomacs/dailies--hours-status-dirty nil
  "Non-nil when hour overlays need refreshing after the current command.")

(defun reomacs/dailies--mark-hours-status-dirty (&rest _)
  "Mark dynamic hour displays stale without parsing during a text edit."
  (setq reomacs/dailies--hours-status-dirty t))

(defun reomacs/dailies--refresh-hours-status-if-dirty ()
  "Refresh dynamic hour displays after the editing command finishes."
  (when reomacs/dailies--hours-status-dirty
    (reomacs/dailies-refresh-week-status)))

(defun reomacs/dailies--add-hours-status-overlay
    (position minutes target-minutes label complete)
  "Display MINUTES/TARGET-MINUTES after POSITION for LABEL.
COMPLETE controls the checkmark independently of the hour count."
  (let* ((base-face (if complete 'success 'shadow))
         (status
          (concat
           (propertize
            (format " — %s" (reomacs/dailies--format-hours minutes))
            'face base-face)
           (propertize
            (format "/%s" (reomacs/dailies--format-hours target-minutes))
            'face 'reomacs/dailies-hours-target-face)
           (propertize (concat "h" (if complete " ✓" ""))
                       'face base-face)))
         (overlay (make-overlay position position)))
    (overlay-put overlay 'after-string status)
    (overlay-put overlay 'help-echo
                 (format "%s logged hours / target" label))
    (push overlay reomacs/dailies--week-status-overlays)))

;;;###autoload
(defun reomacs/dailies-refresh-week-status (&rest _)
  "Refresh dynamic hours displayed after week and date headings."
  (interactive)
  (setq reomacs/dailies--hours-status-dirty nil)
  (mapc #'delete-overlay reomacs/dailies--week-status-overlays)
  (setq reomacs/dailies--week-status-overlays nil)
  (when reomacs/dailies-daily-target-hours
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (let* ((day-target (round (* reomacs/dailies-daily-target-hours 60)))
               (week-target (* 5 day-target)))
          (while (re-search-forward
                  "^\\* Week \\([0-9]\\{4\\}-W[0-9]\\{2\\}\\)$" nil t)
            (let* ((week-heading-end (line-end-position))
                   (week-beg (line-beginning-position 2))
                   (week-end (or (save-excursion
                                   (forward-line 1)
                                   (when (re-search-forward "^\\* " nil t)
                                     (match-beginning 0)))
                                 (point-max)))
                   (week-minutes 0))
              (save-excursion
                (goto-char week-beg)
                (while (re-search-forward org-heading-regexp week-end t)
                  (goto-char (match-beginning 0))
                  (when (reomacs/dailies--date-at-heading)
                    (let* ((done (org-entry-is-done-p))
                           (day-beg (line-beginning-position 2))
                           (day-end (save-excursion
                                      (org-end-of-subtree t)
                                      (point)))
                           (minutes (reomacs/dailies--sum-duration-minutes
                                     day-beg day-end)))
                      (setq week-minutes (+ week-minutes minutes))
                      (reomacs/dailies--add-hours-status-overlay
                       (line-end-position) minutes day-target "Daily" done)))
                  (forward-line 1)))
              (reomacs/dailies--add-hours-status-overlay
               week-heading-end week-minutes week-target "Weekly"
               (>= week-minutes week-target)))))
        (setq reomacs/dailies--week-status-overlays
              (nreverse reomacs/dailies--week-status-overlays))
        (when (called-interactively-p 'interactive)
          (message "Updated %d hours status display%s"
                   (length reomacs/dailies--week-status-overlays)
                   (if (= (length reomacs/dailies--week-status-overlays) 1)
                       "" "s")))))))

(defun reomacs/dailies--configure-week-status ()
  "Configure dynamic week/day status after file-local variables load."
  (if (and reomacs/dailies-daily-target-hours
           (derived-mode-p 'org-mode)
           (reomacs/dailies--buffer-p))
      (progn
        (add-hook 'after-change-functions
                  #'reomacs/dailies--mark-hours-status-dirty nil t)
        (add-hook 'post-command-hook
                  #'reomacs/dailies--refresh-hours-status-if-dirty nil t)
        (reomacs/dailies-refresh-week-status))
    (remove-hook 'after-change-functions
                 #'reomacs/dailies--mark-hours-status-dirty t)
    (remove-hook 'post-command-hook
                 #'reomacs/dailies--refresh-hours-status-if-dirty t)
    (mapc #'delete-overlay reomacs/dailies--week-status-overlays)
    (setq reomacs/dailies--week-status-overlays nil)))

(add-hook 'hack-local-variables-hook #'reomacs/dailies--configure-week-status)

;;;###autoload
(defun reomacs/dailies-day-to-markdown-clipboard (&optional keep-durations)
  "Copy the containing dailies date's entries as Markdown bullets.
By default, remove trailing duration annotations such as {2h} and
{45m @13:00}.  With prefix argument KEEP-DURATIONS, retain them."
  (interactive "P")
  (require 'ox-md)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command requires an Org buffer"))
  (save-excursion
    (let ((date (reomacs/dailies--goto-containing-date-heading)))
      (unless date
        (user-error "Point is not inside a dailies date heading"))
      (let* ((beg (line-beginning-position 2))
             (end (save-excursion
                    (org-end-of-subtree t)
                    (point)))
             (content (buffer-substring-no-properties beg end)))
        (unless keep-durations
          (setq content
                (with-temp-buffer
                  (insert content)
                  (goto-char (point-min))
                  (while (re-search-forward
                          reomacs/dailies--trailing-duration-re nil t)
                    (replace-match ""))
                  (buffer-string))))
        (let ((markdown
               (org-export-string-as
                content 'md t
                '(:with-toc nil :section-numbers nil
                  :with-timestamps nil :with-broken-links t))))
          (setq markdown
                (string-trim
                 (replace-regexp-in-string
                  "^\\([ \t]*[-+*]\\)[ \t]+" "\\1 "
                  (replace-regexp-in-string "\n\\{3,\\}" "\n\n" markdown))))
          (kill-new markdown)
          (message "Copied %s as Markdown%s"
                   date
                   (if keep-durations " with durations" "")))))))
(provide 'reomacs-dailies)

;;; reomacs-dailies.el ends here
