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

(defun reomacs/dailies--date-parts (date)
  "Return DATE as a Gregorian (MONTH DAY YEAR) list."
  (unless (string-match
           "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
           date)
    (user-error "Invalid ISO date: %s" date))
  (let ((parts (list (string-to-number (match-string 2 date))
                     (string-to-number (match-string 3 date))
                     (string-to-number (match-string 1 date)))))
    (unless (calendar-date-is-valid-p parts)
      (user-error "Invalid calendar date: %s" date))
    parts))

(defun reomacs/dailies--date-add-days (date days)
  "Return ISO DATE shifted by calendar DAYS."
  (pcase-let ((`(,month ,day ,year)
               (calendar-gregorian-from-absolute
                (+ (calendar-absolute-from-gregorian
                    (reomacs/dailies--date-parts date))
                   days))))
    (format "%04d-%02d-%02d" year month day)))

(defun reomacs/dailies--date-time (date &optional zone)
  "Return local midnight for ISO DATE in optional time ZONE."
  (pcase-let ((`(,month ,day ,year) (reomacs/dailies--date-parts date)))
    (encode-time 0 0 0 day month year zone)))

(defun reomacs/dailies--utc-search-range (date &optional zone)
  "Return UTC date bounds covering local DATE in optional time ZONE."
  (list (format-time-string "%Y-%m-%d"
                            (reomacs/dailies--date-time date zone) t)
        (format-time-string
         "%Y-%m-%d"
         (time-subtract
          (reomacs/dailies--date-time
           (reomacs/dailies--date-add-days date 1) zone)
          (seconds-to-time 1))
         t)))

(defconst reomacs/dailies--date-heading-title-re
  "\\`<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
  "Matches a date at the start of a metadata-free Org heading title.")

(defun reomacs/dailies--date-at-heading ()
  "Return the ISO date at the current level-two Org heading, or nil.
TODO state, priority, tags and COMMENT metadata are ignored."
  (when (and (org-at-heading-p) (= (org-current-level) 2))
    (let ((title (org-get-heading t t t t)))
      (when (string-match reomacs/dailies--date-heading-title-re title)
        (let ((date (match-string-no-properties 1 title)))
          (condition-case nil
              (progn (reomacs/dailies--date-parts date) date)
            (error nil)))))))

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
      (let* ((last-time (reomacs/dailies--date-time last-date-str))
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

(defun reomacs/dailies--next-workday (from-time)
  "Return the next weekday after FROM-TIME."
  (let ((date (reomacs/dailies--date-add-days
               (format-time-string "%Y-%m-%d" from-time) 1)))
    (while (>= (string-to-number
                (format-time-string "%u" (reomacs/dailies--date-time date)))
               6)
      (setq date (reomacs/dailies--date-add-days date 1)))
    (reomacs/dailies--date-time date)))

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
Keys may be written as \"owner/repo\" for GitHub, \"org/project/repo\"
for Azure DevOps, or any URL/remote containing them.  Both keys and
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

(defun reomacs/dailies--date-on-day-p (iso-date target-date &optional zone)
  "Return non-nil when ISO-DATE falls on TARGET-DATE in optional ZONE."
  (and iso-date
       (not (string-prefix-p "0001-" iso-date))
       (condition-case nil
           (string= target-date
                    (format-time-string "%Y-%m-%d"
                                        (date-to-time iso-date) zone))
         (error nil))))

(defvar reomacs/dailies--username-commands
  '((github . ("gh" "api" "user" "--jq" ".login"))
    (azure . ("az" "account" "show" "--query" "user.name" "--output" "tsv")))
  "Alist mapping forge symbols to argv fetching the current user's login.")

(defun reomacs/dailies--username-command ()
  "Return argv fetching the username for `reomacs/dailies-forge'."
  (or (alist-get reomacs/dailies-forge reomacs/dailies--username-commands)
      (user-error "Unknown dailies forge: %s" reomacs/dailies-forge)))


;; Asynchronous forge runner

(defconst reomacs/dailies--placeholder "⏳ Fetching forge interactions..."
  "Placeholder text shown while an interactions fetch is in progress.")

(defun reomacs/dailies--run-process (command callback)
  "Run argv list COMMAND and call CALLBACK with OUTPUT and ERROR.
ERROR is nil after a successful exit and a diagnostic string otherwise."
  (let ((process-environment (cons "TERM=dumb" process-environment))
        (stdout (generate-new-buffer " *dailies-stdout*"))
        (stderr (generate-new-buffer " *dailies-stderr*"))
        process)
    (condition-case err
        (setq process
              (make-process
               :name (format "dailies-%s"
                             (file-name-nondirectory (car command)))
               :buffer stdout
               :stderr stderr
               :command command
               :connection-type 'pipe
               :coding 'utf-8
               :noquery t
               :sentinel
               (lambda (process _event)
                 (when (and (memq (process-status process) '(exit signal))
                            (not (process-get
                                  process 'reomacs-dailies-finished)))
                   (process-put process 'reomacs-dailies-finished t)
                   (let* ((status (process-exit-status process))
                          (output (with-current-buffer stdout (buffer-string)))
                          (error-output
                           (string-trim
                            (with-current-buffer stderr (buffer-string))))
                          (error-message
                           (unless (and (eq (process-status process) 'exit)
                                        (zerop status))
                             (format
                              "%s exited with status %s%s"
                              (car command) status
                              (if (string-empty-p error-output)
                                  ""
                                (format
                                 ": %s"
                                 (truncate-string-to-width
                                  (car (split-string error-output "\n" t))
                                  200 nil nil t)))))))
                     (unwind-protect
                         (funcall callback output error-message)
                       (when (buffer-live-p stdout) (kill-buffer stdout))
                       (when (buffer-live-p stderr) (kill-buffer stderr))))))))
      (error
       (when (buffer-live-p stdout) (kill-buffer stdout))
       (when (buffer-live-p stderr) (kill-buffer stderr))
       (signal (car err) (cdr err))))
    (ignore-errors (process-send-eof process))
    process))

(defun reomacs/dailies--runner-record-error (state command message)
  "Record MESSAGE for COMMAND in runner STATE."
  (let ((diagnostic (format "%s: %s" (string-join command " ") message)))
    (puthash :errors (cons diagnostic (gethash :errors state)) state)
    (message "Dailies fetch error: %s" diagnostic)))

(defun reomacs/dailies--line-present-p (marker line)
  "Return non-nil when MARKER's buffer already contains LINE."
  (when (marker-buffer marker)
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward
         (concat "^" (regexp-quote line) "$") nil t)))))

(defun reomacs/dailies--runner-finalize (state)
  "Insert collected results and finalize the placeholder for STATE."
  (let* ((marker (gethash :marker state))
         (forge (gethash :forge state))
         (errors (nreverse (gethash :errors state)))
         (results (sort (delete-dups (gethash :results state)) #'string<)))
    (when (and marker (marker-buffer marker))
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (dolist (line results)
            (unless (reomacs/dailies--line-present-p marker line)
              (goto-char marker)
              (insert-before-markers line "\n")))
          (goto-char marker)
          (when (looking-at
                 (regexp-quote (concat "+ " reomacs/dailies--placeholder)))
            (delete-region (point) (line-beginning-position 2))
            (when errors
              (insert (format "+ ERROR: %s interactions, %d request%s failed\n"
                              forge (length errors)
                              (if (= (length errors) 1) "" "s"))))))))
    (message "%s interactions fetch %s"
             forge (if errors
                       (format "finished with %d error%s"
                               (length errors)
                               (if (= (length errors) 1) "" "s"))
                     "complete"))
    (when marker (set-marker marker nil))))

(defun reomacs/dailies--runner-finish (state)
  "Finish one request in runner STATE."
  (let ((pending (1- (gethash :pending state))))
    (puthash :pending pending state)
    (when (zerop pending)
      (reomacs/dailies--runner-finalize state))))

(defun reomacs/dailies--runner-fetch (state command callback)
  "Run COMMAND for STATE and invoke successful CALLBACK with its output."
  (puthash :pending (1+ (gethash :pending state)) state)
  (condition-case err
      (reomacs/dailies--run-process
       command
       (lambda (output process-error)
         (unwind-protect
             (if process-error
                 (reomacs/dailies--runner-record-error
                  state command process-error)
               (condition-case callback-error
                   (funcall callback output)
                 (error
                  (reomacs/dailies--runner-record-error
                   state command (error-message-string callback-error)))
                 (quit
                  (reomacs/dailies--runner-record-error
                   state command "callback quit"))))
           (reomacs/dailies--runner-finish state))))
    (error
     (reomacs/dailies--runner-record-error
      state command (error-message-string err))
     (reomacs/dailies--runner-finish state)
     nil)))

(defun reomacs/dailies--make-runner (date marker)
  "Create an asynchronous interactions runner for DATE at MARKER."
  (let ((state (make-hash-table :test 'eq)))
    (puthash :pending 0 state)
    (puthash :marker marker state)
    (puthash :forge reomacs/dailies-forge state)
    (puthash :errors nil state)
    (puthash :results nil state)
    (list :date date
          :forge reomacs/dailies-forge
          :aliases reomacs/dailies-repo-aliases
          :azure-org reomacs/dailies-azure-org
          :username nil
          :state state
          :fetch (lambda (command callback)
                   (reomacs/dailies--runner-fetch state command callback))
          :emit (lambda (line)
                  (puthash :results
                           (cons line (gethash :results state)) state)))))

(defun reomacs/dailies--json (output)
  "Parse JSON OUTPUT into lists and alists, signaling malformed data."
  (json-parse-string (string-trim output)
                     :array-type 'list :object-type 'alist
                     :null-object nil :false-object nil))

;; GitHub

(defun reomacs/dailies--run-gh-fetch (runner)
  "Start GitHub interaction requests using RUNNER.
Push history remains limited to GitHub's retained Events API window."
  (pcase-let* ((`(,utc-start ,utc-end)
                 (reomacs/dailies--utc-search-range
                  (plist-get runner :date)))
                (date-filter (format "--updated=%s..%s" utc-start utc-end))
                (fetch (plist-get runner :fetch))
                (username (plist-get runner :username)))
    (funcall fetch
             (list "gh" "search" "prs" "--author=@me" date-filter
                   "--limit=1000" "--json"
                   "number,repository,url,createdAt,closedAt,state")
             (lambda (output)
               (reomacs/dailies--process-prs
                runner (reomacs/dailies--json output))))
    (funcall fetch
             (list "gh" "search" "prs" "--reviewed-by=@me" date-filter
                   "--limit=1000" "--json" "number,repository,url")
             (lambda (output)
               (reomacs/dailies--process-reviews
                runner (reomacs/dailies--json output))))
    (funcall fetch
             (list "gh" "search" "issues" "--author=@me" date-filter
                   "--limit=1000" "--json"
                   "number,repository,url,createdAt")
             (lambda (output)
               (reomacs/dailies--process-issues
                runner (reomacs/dailies--json output))))
    (funcall fetch
             (list "gh" "api"
                   (format "/users/%s/events?per_page=100" username)
                   "--paginate" "--slurp" "--jq"
                   (concat "[.[][] | select(.type == \"PushEvent\")"
                           " | {repo: .repo.name, branch: .payload.ref,"
                           " createdAt: .created_at}] | .[]"))
             (lambda (output)
               (reomacs/dailies--process-pushes
                runner (split-string (string-trim output) "\n" t))))))

(defun reomacs/dailies--process-prs (runner prs)
  "Process authored GitHub PRS using RUNNER."
  (-let [(&plist :date :username :aliases :fetch :emit) runner]
    (--each prs
      (-let* [((&alist 'url 'number 'createdAt 'closedAt
                       'repository (&alist 'name 'nameWithOwner)) it)
              (display-name
               (reomacs/dailies--repo-display-name
                aliases nameWithOwner name))
              (props (list :url url :display-name display-name :number number))]
        (when (reomacs/dailies--date-on-day-p createdAt date)
          (funcall emit (reomacs/dailies--format-event 'pr-create props)))
        (when (reomacs/dailies--date-on-day-p closedAt date)
          (funcall
           fetch
           (list "gh" "api"
                 (format "/repos/%s/issues/%s/events?per_page=100"
                         nameWithOwner number)
                 "--paginate" "--slurp" "--jq"
                 (concat "[.[][] | select(.event == \"closed\""
                         " or .event == \"merged\")] | last"
                         " | {actor: .actor.login, event: .event}"))
           (lambda (output)
             (-let* (((&alist 'actor 'event)
                      (reomacs/dailies--json output))
                     (event-type (if (string= event "merged")
                                     'pr-merge 'pr-close))
                     (props (if (and actor
                                     (not (string-equal-ignore-case
                                           actor username)))
                                (plist-put (copy-sequence props) :actor actor)
                              props)))
               (funcall emit
                        (reomacs/dailies--format-event event-type props))))))))))

(defun reomacs/dailies--process-reviews (runner prs)
  "Process reviewed GitHub PRS using RUNNER."
  (-let [(&plist :date :username :aliases :fetch :emit) runner]
    (--each prs
      (-let* [((&alist 'url 'number
                       'repository (&alist 'name 'nameWithOwner)) it)
              (display-name
               (reomacs/dailies--repo-display-name
                aliases nameWithOwner name))]
        (funcall
         fetch
         (list "gh" "api"
               (format "/repos/%s/pulls/%s/reviews?per_page=100"
                       nameWithOwner number)
               "--paginate" "--slurp" "--jq"
               (format
                "[.[][] | select(.user.login == %s) | {submittedAt: .submitted_at}]"
                (json-encode-string username)))
         (lambda (output)
           (when (seq-some
                  (lambda (review)
                    (reomacs/dailies--date-on-day-p
                     (alist-get 'submittedAt review) date))
                  (reomacs/dailies--json output))
             (funcall emit
                      (reomacs/dailies--format-event
                       'pr-review
                       (list :url url :display-name display-name
                             :number number))))))))))

(defun reomacs/dailies--process-issues (runner issues)
  "Process authored GitHub ISSUES using RUNNER."
  (-let [(&plist :date :aliases :emit) runner]
    (--each issues
      (-let* [((&alist 'url 'number 'createdAt
                       'repository (&alist 'name 'nameWithOwner)) it)
              (display-name
               (reomacs/dailies--repo-display-name
                aliases nameWithOwner name))]
        (when (reomacs/dailies--date-on-day-p createdAt date)
          (funcall emit
                   (reomacs/dailies--format-event
                    'issue-create
                    (list :url url :display-name display-name
                          :number number))))))))

(defun reomacs/dailies--process-pushes (runner lines)
  "Process GitHub push event JSON LINES using RUNNER."
  (-let [(&plist :date :aliases :fetch :emit) runner]
    (let ((seen (make-hash-table :test 'equal)))
      (dolist (line lines)
        (-let [(&alist 'repo 'branch 'createdAt)
               (reomacs/dailies--json line)]
          (when (reomacs/dailies--date-on-day-p createdAt date)
            (let* ((branch-name
                    (replace-regexp-in-string "^refs/heads/" "" branch))
                   (key (concat repo "\0" branch-name)))
              (unless (gethash key seen)
                (puthash key t seen)
                (let* ((repo-name (-last-item (split-string repo "/")))
                       (display-name
                        (reomacs/dailies--repo-display-name
                         aliases repo repo-name))
                       (owner (-first-item (split-string repo "/"))))
                  (funcall
                   fetch
                   (list "gh" "api" (format "/repos/%s/pulls" repo)
                         "--method" "GET"
                         "-f" (format "head=%s:%s" owner branch-name)
                         "-f" "state=all" "-f" "per_page=100"
                         "--jq" ".[0] | {number, html_url}")
                   (lambda (output)
                     (-let [(&alist 'number 'html_url)
                            (reomacs/dailies--json output)]
                       (funcall
                        emit
                        (if number
                            (reomacs/dailies--format-event
                             'push-with-pr
                             (list :display-name display-name
                                   :branch branch-name
                                   :url html_url :number number))
                          (reomacs/dailies--format-event
                           'push-no-pr
                           (list :display-name display-name
                                 :branch branch-name))))))))))))))))

;; Azure DevOps

(defun reomacs/dailies--az-org-name (org)
  "Return the organization segment of Azure DevOps URL ORG."
  (car (last (split-string (string-trim-right org "/") "/" t))))

(defun reomacs/dailies--az-pr-browser-url (org pr)
  "Return the browser URL for Azure DevOps PR in ORG."
  (-let* (((&alist 'pullRequestId number
                   'repository (&alist 'name repo-name
                                       'project (&alist 'name project)))
           pr))
    (format "%s/%s/_git/%s/pullrequest/%s"
            (string-trim-right org "/") project repo-name number)))

(defun reomacs/dailies--az-repo-id (org pr)
  "Return the canonical organization/project/repository ID for PR in ORG."
  (-let* (((&alist 'repository (&alist 'name repo-name
                                       'project (&alist 'name project)))
           pr))
    (string-join
     (list (reomacs/dailies--az-org-name org) project repo-name) "/")))

(defun reomacs/dailies--az-threads-command (org pr)
  "Return argv fetching Azure discussion threads for PR in ORG."
  (-let* (((&alist 'pullRequestId number
                   'repository (&alist 'id repo-id
                                       'project (&alist 'name project)))
           pr))
    (list "az" "devops" "invoke" "--org" org
          "--area" "git" "--resource" "pullRequestThreads"
          "--route-parameters"
          (format "project=%s" project)
          (format "repositoryId=%s" repo-id)
          (format "pullRequestId=%s" number)
          "-o" "json")))

(defun reomacs/dailies--identity-equal-p (left right)
  "Return non-nil when identity strings LEFT and RIGHT match."
  (and (stringp left) (stringp right)
       (string-equal-ignore-case left right)))

(defun reomacs/dailies--az-review-candidate-p (pr username date)
  "Return non-nil when PR merits the bounded Azure thread scan.
The Azure CLI cannot query comments by author and date.  To avoid an
organization-wide request per historical PR, scan PRs where USERNAME
is currently assigned or whose lifecycle changed on DATE."
  (let ((creator (alist-get 'uniqueName (alist-get 'createdBy pr))))
    (and (not (reomacs/dailies--identity-equal-p username creator))
         (or (seq-some
              (lambda (reviewer)
                (reomacs/dailies--identity-equal-p
                 username (alist-get 'uniqueName reviewer)))
              (alist-get 'reviewers pr))
             (reomacs/dailies--date-on-day-p
              (alist-get 'creationDate pr) date)
             (reomacs/dailies--date-on-day-p
              (alist-get 'closedDate pr) date)))))

(defun reomacs/dailies--process-az-prs (runner prs)
  "Emit events for Azure DevOps PR alists PRS using RUNNER."
  (-let* (((&plist :date :aliases :azure-org :username :emit :fetch)
           runner))
    (--each prs
      (-let* ((display-name
               (reomacs/dailies--repo-display-name
                aliases (reomacs/dailies--az-repo-id azure-org it)
                (alist-get 'name (alist-get 'repository it))))
              (props (list :url
                           (reomacs/dailies--az-pr-browser-url azure-org it)
                           :display-name display-name
                           :number (alist-get 'pullRequestId it)))
              (status (alist-get 'status it))
              (mine
               (reomacs/dailies--identity-equal-p
                username
                (alist-get 'uniqueName (alist-get 'createdBy it)))))
        (when (and mine
                   (reomacs/dailies--date-on-day-p
                    (alist-get 'creationDate it) date))
          (funcall emit (reomacs/dailies--format-event 'pr-create props)))
        (when (and mine
                   (member status '("completed" "abandoned"))
                   (reomacs/dailies--date-on-day-p
                    (alist-get 'closedDate it) date))
          (funcall emit
                   (reomacs/dailies--format-event
                    (if (string= status "completed") 'pr-merge 'pr-close)
                    props)))
        (when (reomacs/dailies--az-review-candidate-p it username date)
          (funcall
           fetch (reomacs/dailies--az-threads-command azure-org it)
           (lambda (output)
             (reomacs/dailies--process-az-threads
              runner props (alist-get 'value
                                      (reomacs/dailies--json output))))))))))

(defun reomacs/dailies--process-az-threads (runner props threads)
  "Emit PROPS when THREADS contain my comments on RUNNER's date."
  (-let [(&plist :date :username :emit) runner]
    (when
        (seq-some
         (lambda (thread)
           (seq-some
            (lambda (comment)
              (and (reomacs/dailies--date-on-day-p
                    (alist-get 'publishedDate comment) date)
                   (reomacs/dailies--identity-equal-p
                    (alist-get 'uniqueName (alist-get 'author comment))
                    username)
                   (not (equal (alist-get 'commentType comment) "system"))))
            (alist-get 'comments thread)))
         threads)
      (funcall emit (reomacs/dailies--format-event 'pr-review props)))))

(defun reomacs/dailies--fetch-azure-pr-page (runner project skip)
  "Fetch one 500-PR page for PROJECT at SKIP using RUNNER."
  (let ((fetch (plist-get runner :fetch))
        (org (plist-get runner :azure-org)))
    (funcall
     fetch
     (list "az" "repos" "pr" "list" "--org" org
           "--project" project "--status" "all"
           "--top" "500" "--skip" (number-to-string skip) "-o" "json")
     (lambda (output)
       (let ((prs (reomacs/dailies--json output)))
         (reomacs/dailies--process-az-prs runner prs)
         (when (= (length prs) 500)
           (reomacs/dailies--fetch-azure-pr-page
            runner project (+ skip 500))))))))

(defun reomacs/dailies--run-azure-fetch (runner)
  "Start bounded Azure DevOps interaction requests using RUNNER.
Pushes have no queryable Azure feed and are not fetched."
  (let ((fetch (plist-get runner :fetch))
        (org (plist-get runner :azure-org)))
    (funcall
     fetch
     (list "az" "devops" "project" "list" "--org" org
           "--top" "1000" "-o" "json")
     (lambda (output)
       (let ((projects (alist-get 'value
                                  (reomacs/dailies--json output))))
         (when (= (length projects) 1000)
           (error "Azure project list reached its 1000-project safety limit"))
         (dolist (project projects)
           (reomacs/dailies--fetch-azure-pr-page
            runner (alist-get 'name project) 0)))))))

(defun reomacs/dailies--run-forge-fetch (runner)
  "Dispatch RUNNER to its configured forge."
  (pcase (plist-get runner :forge)
    ('github (reomacs/dailies--run-gh-fetch runner))
    ('azure (reomacs/dailies--run-azure-fetch runner))
    (forge (error "Unknown dailies forge: %s" forge))))

(defun reomacs/dailies--fetch-interactions (date marker)
  "Fetch interactions for DATE and insert results before MARKER."
  (let* ((runner (reomacs/dailies--make-runner date marker))
         (fetch (plist-get runner :fetch)))
    (funcall
     fetch (reomacs/dailies--username-command)
     (lambda (output)
       (let ((username (string-trim output)))
         (when (string-empty-p username)
           (error "Forge username command returned no value"))
         (plist-put runner :username username)
         (reomacs/dailies--run-forge-fetch runner))))))

(defun reomacs/dailies--previous-workday (from-time)
  "Return the previous weekday before FROM-TIME."
  (let ((date (reomacs/dailies--date-add-days
               (format-time-string "%Y-%m-%d" from-time) -1)))
    (while (>= (string-to-number
                (format-time-string "%u" (reomacs/dailies--date-time date)))
               6)
      (setq date (reomacs/dailies--date-add-days date -1)))
    (reomacs/dailies--date-time date)))

(defun reomacs/dailies--heading-date ()
  "Return the containing date heading as an Emacs time value, or nil."
  (save-excursion
    (when-let* ((date (reomacs/dailies--goto-containing-date-heading)))
      (reomacs/dailies--date-time date))))

(defun reomacs/dailies--target-date (days-back)
  "Return the interaction date selected by DAYS-BACK."
  (if (null days-back)
      (if-let* ((heading-time (reomacs/dailies--heading-date)))
          (format-time-string
           "%Y-%m-%d" (reomacs/dailies--previous-workday heading-time))
        (user-error "No date heading found above point"))
    (unless (and (integerp days-back) (>= days-back 0))
      (user-error "Days back must be a non-negative integer"))
    (reomacs/dailies--date-add-days
     (format-time-string "%Y-%m-%d") (- days-back))))

;;;###autoload
(defun reomacs/dailies-insert-interactions (days-back)
  "Insert git forge interactions asynchronously.
Queries the forge configured by `reomacs/dailies-forge' and replaces a
placeholder when requests finish.  DAYS-BACK nil uses the previous
workday, while zero uses today."
  (interactive
   (list
    (let ((input
           (read-string
            "Days back (empty for smart yesterday, 0 for today): ")))
      (cond
       ((string-empty-p input) nil)
       ((string-match-p "\\`[0-9]+\\'" input) (string-to-number input))
       (t (user-error "Days back must be a non-negative integer"))))))
  (unless (and (derived-mode-p 'org-mode)
               (reomacs/dailies--buffer-p))
    (user-error "This command requires the configured dailies buffer"))
  (let ((target-date (reomacs/dailies--target-date days-back))
        (marker (point-marker)))
    (insert "+ " reomacs/dailies--placeholder "\n")
    (set-marker marker (line-beginning-position 0))
    (set-marker-insertion-type marker t)
    (message "Fetching %s interactions for %s..."
             reomacs/dailies-forge target-date)
    (reomacs/dailies--fetch-interactions target-date marker)))

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
  (concat "[ \\t]+" reomacs/dailies--duration-re "[ \\t]*$")
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
