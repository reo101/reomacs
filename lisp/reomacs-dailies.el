;;; reomacs-dailies.el --- Work dailies helpers -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (dash "2.20.0"))
;; Keywords: convenience, outlines

;;; Commentary:

;; Maintain flat Org work dailies, fetch forge interactions, insert forge
;; references, display logged hours, and copy daily entries as Markdown.
;;
;; Configure per-journal settings (forge, project, aliases, targets) as
;; file-local variables in the dailies file.  Emacs only honors a Local
;; Variables block within the last 3000 characters of a file, so keep the
;; block at the end of the journal.

;;; Code:

(require 'calendar)
(require 'dash)
(require 'json)
(require 'org)
(require 'org-element)
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
          (when (or (null last-date-str)
                    (string< last-date-str date)
                    (and (string= last-date-str date)
                         (> (point) last-date-pos)))
            (setq last-date-pos (point)
                  last-date-str date)))
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

(defcustom reomacs/dailies-azure-project nil
  "Azure DevOps project used for work-item and PR reference completion."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'reo101
  :safe (lambda (value) (and (stringp value) (not (string-blank-p value)))))

(defcustom reomacs/dailies-default-repository nil
  "Repository identity preselected by the forge reference picker.
Use an Azure organization/project/repository identity or GitHub owner/repo."
  :type '(choice (const :tag "All repositories" nil) string)
  :group 'reo101
  :safe (lambda (value) (and (stringp value) (not (string-blank-p value)))))

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

(defun reomacs/dailies--timestamp-date (iso-date &optional zone)
  "Return ISO-DATE's calendar date in optional ZONE, or nil."
  (when (and iso-date (not (string-prefix-p "0001-" iso-date)))
    (condition-case nil
        (format-time-string "%Y-%m-%d" (date-to-time iso-date) zone)
      (error nil))))

(defun reomacs/dailies--date-on-day-p (iso-date target-date &optional zone)
  "Return non-nil when ISO-DATE falls on TARGET-DATE in optional ZONE."
  (equal target-date (reomacs/dailies--timestamp-date iso-date zone)))

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

(defcustom reomacs/dailies-process-concurrency 8
  "Maximum simultaneous external dailies processes."
  :type 'integer
  :group 'reo101)

(defun reomacs/dailies--make-request-pool (on-error on-idle)
  "Create a concurrency-limited request pool.
Call ON-ERROR with command and diagnostic; call ON-IDLE after all work ends."
  (let ((pool (make-hash-table :test 'eq)))
    (puthash :limit (max 1 reomacs/dailies-process-concurrency) pool)
    (puthash :active 0 pool)
    (puthash :pending 0 pool)
    (puthash :queue nil pool)
    (puthash :processes nil pool)
    (puthash :holding nil pool)
    (puthash :closed nil pool)
    (puthash :finished nil pool)
    (puthash :on-error on-error pool)
    (puthash :on-idle on-idle pool)
    pool))

(defun reomacs/dailies--request-pool-report-error (pool command message)
  "Report MESSAGE for COMMAND through POOL's error callback."
  (condition-case err
      (funcall (gethash :on-error pool) command message)
    (error
     (message "Dailies request error handler failed: %s"
              (error-message-string err)))))

(defun reomacs/dailies--request-pool-maybe-finish (pool)
  "Finalize POOL once no queued or active work remains."
  (when (and (not (gethash :closed pool))
             (not (gethash :holding pool))
             (not (gethash :finished pool))
             (zerop (gethash :pending pool))
             (zerop (gethash :active pool))
             (null (gethash :queue pool)))
    (puthash :finished t pool)
    (condition-case err
        (funcall (gethash :on-idle pool))
      (error
       (message "Dailies request finalizer failed: %s"
                (error-message-string err))))))

(defun reomacs/dailies--request-pool-complete (pool request output process-error)
  "Complete REQUEST in POOL with OUTPUT or PROCESS-ERROR."
  (unless (gethash :closed pool)
    (unwind-protect
        (if process-error
            (reomacs/dailies--request-pool-report-error
             pool (plist-get request :command) process-error)
          (condition-case err
              (funcall (plist-get request :callback) output)
            (error
             (reomacs/dailies--request-pool-report-error
              pool (plist-get request :command) (error-message-string err)))
            (quit
             (reomacs/dailies--request-pool-report-error
              pool (plist-get request :command) "callback quit"))))
      (puthash :active (1- (gethash :active pool)) pool)
      (puthash :pending (1- (gethash :pending pool)) pool)
      (reomacs/dailies--request-pool-pump pool)
      (reomacs/dailies--request-pool-maybe-finish pool))))

(defun reomacs/dailies--request-pool-pump (pool)
  "Start queued requests in POOL up to its concurrency limit."
  (while (and (not (gethash :closed pool))
              (not (gethash :holding pool))
              (gethash :queue pool)
              (< (gethash :active pool) (gethash :limit pool)))
    (let* ((request (pop (gethash :queue pool)))
           (command (plist-get request :command)))
      (puthash :active (1+ (gethash :active pool)) pool)
      (condition-case err
          (let ((process
                 (reomacs/dailies--run-process
                  command
                  (lambda (output process-error)
                    (reomacs/dailies--request-pool-complete
                     pool request output process-error)))))
            (puthash :processes
                     (cons process (gethash :processes pool)) pool))
        (error
         (reomacs/dailies--request-pool-complete
          pool request "" (error-message-string err)))))))

(defun reomacs/dailies--request-pool-submit (pool command callback)
  "Submit COMMAND and successful CALLBACK to POOL."
  (when (or (gethash :closed pool) (gethash :finished pool))
    (error "Cannot submit to a finished dailies request pool"))
  (puthash :pending (1+ (gethash :pending pool)) pool)
  (puthash :queue
           (cons (list :command command :callback callback)
                 (gethash :queue pool))
           pool)
  (unless (gethash :holding pool)
    (reomacs/dailies--request-pool-pump pool)))

(defun reomacs/dailies--request-pool-close (pool)
  "Cancel queued and running work in POOL."
  (puthash :closed t pool)
  (puthash :queue nil pool)
  (dolist (process (gethash :processes pool))
    (when (process-live-p process)
      (delete-process process))))

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

(defun reomacs/dailies--make-runner (date marker)
  "Create an asynchronous interactions runner for DATE at MARKER."
  (let ((state (make-hash-table :test 'eq)) pool)
    (puthash :marker marker state)
    (puthash :forge reomacs/dailies-forge state)
    (puthash :errors nil state)
    (puthash :results nil state)
    (setq pool
          (reomacs/dailies--make-request-pool
           (lambda (command message)
             (reomacs/dailies--runner-record-error state command message))
           (lambda () (reomacs/dailies--runner-finalize state))))
    (list :date date
          :forge reomacs/dailies-forge
          :aliases reomacs/dailies-repo-aliases
          :azure-org reomacs/dailies-azure-org
          :username nil
          :state state
          :pool pool
          :fetch (lambda (command callback)
                   (reomacs/dailies--request-pool-submit
                    pool command callback))
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

(defun reomacs/dailies--az-pr-open-on-day-p (pr date)
  "Return non-nil when Azure PR existed and was open during DATE."
  (let ((created (reomacs/dailies--timestamp-date
                  (alist-get 'creationDate pr)))
        (closed (reomacs/dailies--timestamp-date
                 (alist-get 'closedDate pr))))
    (and created
         (not (string< date created))
         (or (null closed) (not (string< closed date))))))

(defun reomacs/dailies--az-review-candidate-p (pr username date)
  "Return non-nil when PR may contain a USERNAME review on DATE.
Azure cannot query comments by author or date, so every PR open during
DATE must have its discussion threads inspected for complete discovery."
  (and (not (reomacs/dailies--identity-equal-p
             username (alist-get 'uniqueName (alist-get 'createdBy pr))))
       (reomacs/dailies--az-pr-open-on-day-p pr date)))

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

(defun reomacs/dailies--fetch-azure-project-page (runner skip)
  "Fetch one 500-project Azure page for RUNNER starting at SKIP."
  (let ((fetch (plist-get runner :fetch))
        (org (plist-get runner :azure-org)))
    (funcall
     fetch
     (list "az" "devops" "project" "list" "--org" org
           "--top" "500" "--skip" (number-to-string skip) "-o" "json")
     (lambda (output)
       (let ((projects (alist-get 'value (reomacs/dailies--json output))))
         (dolist (project projects)
           (reomacs/dailies--fetch-azure-pr-page
            runner (alist-get 'name project) 0))
         (when (= (length projects) 500)
           (reomacs/dailies--fetch-azure-project-page
            runner (+ skip 500))))))))

(defun reomacs/dailies--run-azure-fetch (runner)
  "Start paginated Azure DevOps interaction requests using RUNNER.
Pushes have no queryable Azure feed and are not fetched."
  (reomacs/dailies--fetch-azure-project-page runner 0))

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
workday, while zero uses today.  GitHub push history is limited to its
retained Events API window.  Azure review discovery checks every PR
that was open on the requested date with concurrency-limited thread scans."
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
;; Forge reference picker

(defvar reomacs/dailies-reference-history nil
  "Minibuffer history for dailies forge references.")

(defvar reomacs/dailies-reference-repository-history nil
  "Minibuffer history for dailies repository scopes.")

(defun reomacs/dailies--reference-title (value)
  "Return VALUE as safe single-line Org link description text."
  (string-replace
   "]" ")"
   (string-replace
    "[" "("
    (replace-regexp-in-string "[\n\r\t ]+" " " (string-trim (or value ""))))))

(defun reomacs/dailies--reference-description (name kind number title)
  "Format a reference to NAME's KIND NUMBER with TITLE."
  (let ((title (reomacs/dailies--reference-title title)))
    (format "~%s~ %s %s%s" name kind number
            (if (string-empty-p title) "" (concat ": " title)))))

(defun reomacs/dailies--reference-url-forge (url)
  "Return the forge recognized from reference URL, or nil."
  (let ((case-fold-search t))
    (cond
     ((string-match-p
       "github\\.com/[^/]+/[^/]+/\\(?:issues\\|pull\\)/[0-9]+" url)
      'github)
     ((and (string-match-p "\\(?:dev\\.azure\\.com\\|visualstudio\\.com\\)" url)
           (string-match-p
            "\\(?:/pullrequest/[0-9]+\\|/_workitems/edit/[0-9]+\\)" url))
      'azure))))

(defun reomacs/dailies--reference-url-repository (url)
  "Return the repository identity encoded in reference URL, or nil."
  (let ((case-fold-search t))
    (cond
     ((string-match
       "github\\.com/\\([^/]+/[^/]+\\)/\\(?:issues\\|pull\\)/[0-9]+" url)
      (match-string 1 url))
     ((string-match
       "dev\\.azure\\.com/[^/]+/[^/]+/_git/\\([^/]+\\)/pullrequest/[0-9]+"
       url)
      (match-string 1 url)))))

(defun reomacs/dailies--reference-matches-source-p (url source)
  "Return non-nil when reference URL belongs to SOURCE."
  (and (eq (reomacs/dailies--reference-url-forge url)
           (plist-get source :forge))
       (let ((selected (plist-get source :repository))
             (actual (reomacs/dailies--reference-url-repository url)))
         (or (null selected)
             (null actual)
             (string-equal-ignore-case selected actual)))))

(defvar-local reomacs/dailies--reference-index nil
  "Cached local forge references in the current dailies buffer.")

(defvar-local reomacs/dailies--reference-index-tick nil
  "Buffer modification tick corresponding to the local reference index.")

(defun reomacs/dailies--local-reference-index ()
  "Return local forge references, rebuilding the buffer cache when stale."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eql tick reomacs/dailies--reference-index-tick)
      (save-restriction
        (widen)
        (let (candidates)
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (link)
              (let* ((url (org-element-property :raw-link link))
                     (position (org-element-property :begin link)))
                (when (and url
                           (reomacs/dailies--reference-url-forge url)
                           (save-excursion
                             (goto-char position)
                             (reomacs/dailies--goto-containing-date-heading)))
                  (let* ((description
                          (if-let* ((beg
                                     (org-element-property :contents-begin link))
                                    (end
                                     (org-element-property :contents-end link)))
                              (buffer-substring-no-properties beg end)
                            url))
                         (description
                          (reomacs/dailies--reference-title description)))
                    (push (list :url url
                                :display (concat "Recent  " description)
                                :link (org-link-make-string url description)
                                :position position)
                          candidates))))))
          (setq reomacs/dailies--reference-index
                (sort candidates
                      (lambda (left right)
                        (> (plist-get left :position)
                           (plist-get right :position))))
                reomacs/dailies--reference-index-tick tick))))
    reomacs/dailies--reference-index))

(defun reomacs/dailies--recent-reference-candidates (sources)
  "Return up to 30 cached local reference candidates matching SOURCES."
  (seq-take
   (reomacs/dailies--deduplicate-reference-candidates
    (seq-filter
     (lambda (candidate)
       (seq-some
        (lambda (source)
          (reomacs/dailies--reference-matches-source-p
           (plist-get candidate :url) source))
        sources))
     (reomacs/dailies--local-reference-index)))
   30))

(defun reomacs/dailies--parse-github-reference-candidates (output aliases)
  "Parse GitHub reference candidates from OUTPUT using ALIASES."
  (mapcar
   (lambda (item)
     (let* ((repository (alist-get 'repository item))
            (repo-id (alist-get 'nameWithOwner repository))
            (repo-name (alist-get 'name repository))
            (name (reomacs/dailies--repo-display-name
                   aliases repo-id repo-name))
            (number (alist-get 'number item))
            (kind (if (alist-get 'isPullRequest item) "PR" "Issue"))
            (title (reomacs/dailies--reference-title
                    (alist-get 'title item)))
            (url (alist-get 'url item))
            (description
             (reomacs/dailies--reference-description
              name kind number title)))
       (list :url url
             :display (format "GitHub %s  %s #%s  %s"
                              kind repo-id number title)
             :link (org-link-make-string url description)
             :updated (alist-get 'updatedAt item))))
   (reomacs/dailies--json output)))

(defun reomacs/dailies--parse-azure-work-reference-candidates
    (output org project)
  "Parse Azure work-item candidates from OUTPUT for ORG and PROJECT."
  (mapcar
   (lambda (item)
     (let* ((fields (alist-get 'fields item))
            (number (alist-get 'id item))
            (item-project (or (alist-get 'System.TeamProject fields) project))
            (kind (or (alist-get 'System.WorkItemType fields) "Work item"))
            (title (reomacs/dailies--reference-title
                    (alist-get 'System.Title fields)))
            (url (format "%s/%s/_workitems/edit/%s"
                         (string-trim-right org "/") item-project number))
            (description
             (reomacs/dailies--reference-description
              item-project kind number title)))
       (list :url url
             :display (format "Azure ticket  %s %s %s  %s"
                              item-project kind number title)
             :link (org-link-make-string url description)
             :updated (alist-get 'System.ChangedDate fields))))
   (reomacs/dailies--json output)))

(defun reomacs/dailies--parse-azure-pr-reference-candidates
    (output org aliases)
  "Parse Azure PR candidates from OUTPUT for ORG using ALIASES."
  (mapcar
   (lambda (pr)
     (let* ((repository (alist-get 'repository pr))
            (name (reomacs/dailies--repo-display-name
                   aliases (reomacs/dailies--az-repo-id org pr)
                   (alist-get 'name repository)))
            (number (alist-get 'pullRequestId pr))
            (title (reomacs/dailies--reference-title
                    (alist-get 'title pr)))
            (url (reomacs/dailies--az-pr-browser-url org pr))
            (description
             (reomacs/dailies--reference-description
              name "PR" number title)))
       (list :url url
             :display (format "Azure PR  %s #%s  %s" name number title)
             :link (org-link-make-string url description)
             :updated (or (alist-get 'closedDate pr)
                          (alist-get 'creationDate pr)))))
   (reomacs/dailies--json output)))

(defun reomacs/dailies--azure-pr-participant-p (pr username)
  "Return non-nil when USERNAME created or reviews Azure PR."
  (or (reomacs/dailies--identity-equal-p
       username (alist-get 'uniqueName (alist-get 'createdBy pr)))
      (seq-some
       (lambda (reviewer)
         (reomacs/dailies--identity-equal-p
          username (alist-get 'uniqueName reviewer)))
       (alist-get 'reviewers pr))))

(defun reomacs/dailies--azure-linked-work-item-requests
    (prs org project username)
  "Build work-item requests from participating PRS.
ORG and PROJECT identify the Azure source; USERNAME identifies participation."
  (mapcar
   (lambda (pr)
     (list
      :command
      (list "az" "repos" "pr" "work-item" "list" "--org" org
            "--id" (number-to-string (alist-get 'pullRequestId pr)) "-o" "json")
      :parser
      (lambda (work-output)
        (reomacs/dailies--parse-azure-work-reference-candidates
         work-output org project))))
   (seq-filter
    (lambda (pr)
      (reomacs/dailies--azure-pr-participant-p pr username))
    prs)))

(defun reomacs/dailies--azure-pr-reference-request
    (org project repository aliases username &optional skip)
  "Build a paginated Azure PR request for ORG and PROJECT.
Limit it to REPOSITORY, use ALIASES for display, find links for USERNAME,
and start at optional SKIP."
  (let ((skip (or skip 0)))
    (list
     :command
     (append (list "az" "repos" "pr" "list" "--org" org
                   "--project" project)
             (when repository (list "--repository" repository))
             (list "--status" "all" "--top" "100"
                   "--skip" (number-to-string skip) "-o" "json"))
     :parser
     (lambda (output)
       (reomacs/dailies--parse-azure-pr-reference-candidates
        output org aliases))
     :followups
     (lambda (output)
       (let ((prs (reomacs/dailies--json output)))
         (append
          (reomacs/dailies--azure-linked-work-item-requests
           prs org project username)
          (when (= (length prs) 100)
            (list
             (reomacs/dailies--azure-pr-reference-request
              org project repository aliases username (+ skip 100))))))))))

(defun reomacs/dailies--reference-requests (sources)
  "Return remote reference requests for SOURCES."
  (let (requests)
    (dolist (source sources)
      (pcase (plist-get source :forge)
        ('github
         (let ((repository (plist-get source :repository)))
           (push
            (list
             :command
             (append
              (list "gh" "search" "issues" "--involves=@me" "--include-prs")
              (when repository (list (format "--repo=%s" repository)))
              (list "--sort=updated" "--order=desc" "--limit=1000" "--json"
                    (concat "number,repository,url,title,updatedAt,"
                            "isPullRequest,state")))
             :parser
             (let ((aliases reomacs/dailies-repo-aliases))
               (lambda (output)
                 (reomacs/dailies--parse-github-reference-candidates
                  output aliases))))
            requests)))
        ('azure
         (let* ((org reomacs/dailies-azure-org)
                (project (plist-get source :project))
                (repository (plist-get source :repository))
                (aliases reomacs/dailies-repo-aliases)
                (quoted-project (string-replace "'" "''" project))
                (wiql
                 (concat
                  "SELECT [System.Id], [System.Title], [System.WorkItemType], "
                  "[System.State], [System.ChangedDate] FROM WorkItems WHERE "
                  "[System.TeamProject] = '" quoted-project "' AND "
                  "[System.ChangedDate] >= @Today - 60 AND "
                  "(EVER [System.AssignedTo] = @Me OR "
                  "[System.CreatedBy] = @Me OR EVER [System.ChangedBy] = @Me) "
                  "ORDER BY [System.ChangedDate] DESC")))
           (push
            (list
             :command (list "az" "boards" "query" "--org" org
                            "--project" project "--wiql" wiql "-o" "json")
             :parser
             (lambda (output)
               (reomacs/dailies--parse-azure-work-reference-candidates
                output org project)))
            requests)
           (push
            (list
             :command
             (copy-sequence
              (alist-get 'azure reomacs/dailies--username-commands))
             :followups
             (lambda (output)
               (let ((username (string-trim output)))
                 (when (string-empty-p username)
                   (error "Azure username command returned no value"))
                 (list
                  (reomacs/dailies--azure-pr-reference-request
                   org project repository aliases username)))))
            requests)))
        (forge (error "Unknown dailies forge: %s" forge))))
    (nreverse requests)))

(defun reomacs/dailies--invalidate-completion-ui ()
  "Invalidate candidates in the active completion frontend."
  ;; Vertico has no public push-refresh API.  Its own extensions call this
  ;; internal entry point too, so confine the compatibility boundary here.
  (cond
   ((and (local-variable-p 'vertico--input)
         (fboundp 'vertico--exhibit))
    (set 'vertico--input nil)
    (funcall (symbol-function 'vertico--exhibit)))
   ((fboundp 'completion--flush-all-sorted-completions)
    (completion--flush-all-sorted-completions))
   (t (redisplay))))

(defun reomacs/dailies--refresh-reference-minibuffer (state)
  "Refresh the active completion UI from reference STATE."
  (when-let* ((buffer (plist-get state :minibuffer))
              ((not (plist-get state :closed)))
              (window (active-minibuffer-window))
              ((eq buffer (window-buffer window))))
    (with-current-buffer buffer
      (reomacs/dailies--invalidate-completion-ui)
      (when (and (when-let* ((pool (plist-get state :pool)))
                   (gethash :finished pool))
                 (null (plist-get state :candidates)))
        (minibuffer-message "No references found; remote fetches failed")))))

(defun reomacs/dailies--update-reference-state (state candidates &optional error)
  "Merge CANDIDATES or ERROR into reference STATE and refresh completion."
  (unless (plist-get state :closed)
    (when candidates
      (plist-put state :remote
                 (nconc candidates (plist-get state :remote))))
    (when error
      (plist-put state :errors (cons error (plist-get state :errors))))
    (let ((remote
           (sort (plist-get state :remote)
                 (lambda (left right)
                   (string-lessp
                    (or (plist-get right :updated) "")
                    (or (plist-get left :updated) ""))))))
      (plist-put state :remote remote)
      (plist-put
       state :candidates
       (reomacs/dailies--deduplicate-reference-candidates
        (append (plist-get state :local) remote))))
    (run-at-time 0 nil #'reomacs/dailies--refresh-reference-minibuffer state)))

(defun reomacs/dailies--submit-reference-request (pool request state)
  "Submit reference REQUEST to POOL and stream results into STATE."
  (reomacs/dailies--request-pool-submit
   pool (plist-get request :command)
   (lambda (output)
     (when-let* ((parser (plist-get request :parser)))
       (reomacs/dailies--update-reference-state
        state (funcall parser output)))
     (when-let* ((builder (plist-get request :followups)))
       (dolist (followup (funcall builder output))
         (reomacs/dailies--submit-reference-request
          pool followup state))))))

(defun reomacs/dailies--start-reference-fetch (requests state)
  "Start asynchronous REQUESTS and stream their results into STATE."
  (let ((pool
         (reomacs/dailies--make-request-pool
          (lambda (command message)
            (reomacs/dailies--update-reference-state
             state nil (format "%s: %s" (string-join command " ") message)))
          (lambda ()
            (run-at-time
             0 nil #'reomacs/dailies--refresh-reference-minibuffer state)))))
    (plist-put state :pool pool)
    (puthash :holding t pool)
    (unwind-protect
        (dolist (request requests)
          (reomacs/dailies--submit-reference-request pool request state))
      (puthash :holding nil pool)
      (reomacs/dailies--request-pool-pump pool)
      (reomacs/dailies--request-pool-maybe-finish pool))))

(defun reomacs/dailies--azure-reference-source (project)
  "Return an Azure reference source for PROJECT."
  (unless (and (stringp project) (not (string-blank-p project)))
    (user-error
     "Set reomacs/dailies-azure-project or use C-u to choose a project"))
  (list :forge 'azure :project project))

(defun reomacs/dailies--reference-sources (prompt)
  "Return reference sources, prompting for scope when PROMPT is non-nil."
  (if (not prompt)
      (list
       (pcase reomacs/dailies-forge
         ('azure
          (reomacs/dailies--azure-reference-source
           reomacs/dailies-azure-project))
         ('github (list :forge 'github))
         (forge (user-error "Unknown dailies forge: %s" forge))))
    (pcase (completing-read
            "Reference scope: "
            '("Current forge" "All forges" "Azure project..." "GitHub only")
            nil t nil nil "Current forge")
      ("Current forge" (reomacs/dailies--reference-sources nil))
      ("All forges"
       (list
        (reomacs/dailies--azure-reference-source
         (or reomacs/dailies-azure-project
             (read-string "Azure project: ")))
        (list :forge 'github)))
      ("Azure project..."
       (list
        (reomacs/dailies--azure-reference-source
         (read-string "Azure project: " reomacs/dailies-azure-project))))
      ("GitHub only" (list (list :forge 'github))))))

(defun reomacs/dailies--repository-for-source (identity source)
  "Resolve repository IDENTITY for reference SOURCE, or return nil."
  (when identity
    (let* ((parts (split-string
                   (reomacs/dailies--canonical-repo-id identity) "/" t))
           (count (length parts)))
      (pcase (plist-get source :forge)
        ('azure
         (cond
          ((= count 1) (car parts))
          ((and (= count 2)
                (string-equal-ignore-case
                 (car parts) (plist-get source :project)))
           (cadr parts))
          ((and (>= count 3)
                (string-equal-ignore-case
                 (nth (- count 2) parts) (plist-get source :project)))
           (car (last parts)))))
        ('github
         (when (= count 2)
           (string-join parts "/")))))))

(defun reomacs/dailies--apply-reference-repository (sources identity)
  "Apply repository IDENTITY to each compatible source in SOURCES."
  (mapcar
   (lambda (source)
     (let ((source (copy-sequence source)))
       (when-let* ((repository
                    (reomacs/dailies--repository-for-source identity source)))
         (setq source (plist-put source :repository repository)))
       source))
   sources))

(defun reomacs/dailies--reference-repository-choices (sources)
  "Return repository completion choices compatible with SOURCES."
  (let ((choices
         (delq
          nil
          (mapcar
           (lambda (alias)
             (when (seq-some
                    (lambda (source)
                      (reomacs/dailies--repository-for-source
                       (car alias) source))
                    sources)
               (cons (format "%s  %s" (cdr alias) (car alias))
                     (car alias))))
           reomacs/dailies-repo-aliases))))
    (when (and reomacs/dailies-default-repository
               (seq-some
                (lambda (source)
                  (reomacs/dailies--repository-for-source
                   reomacs/dailies-default-repository source))
                sources)
               (not (seq-find
                     (lambda (choice)
                       (equal (cdr choice)
                              reomacs/dailies-default-repository))
                     choices)))
      (push (cons reomacs/dailies-default-repository
                  reomacs/dailies-default-repository)
            choices))
    (let ((default
           (seq-find
            (lambda (choice)
              (equal (cdr choice) reomacs/dailies-default-repository))
            choices)))
      (append (and default (list default))
              (delete default choices)
              (list (cons "All repositories" nil))))))

(defun reomacs/dailies--prompt-reference-repository (sources)
  "Prompt for a repository and apply it to compatible SOURCES."
  (let* ((choices (reomacs/dailies--reference-repository-choices sources))
         (choice
          (completing-read
           "Repository for references: " (mapcar #'car choices)
           nil nil nil 'reomacs/dailies-reference-repository-history
           (caar choices)))
         (entry (assoc choice choices))
         (identity (if entry (cdr entry) (string-trim choice))))
    (reomacs/dailies--apply-reference-repository
     sources (unless (string-empty-p (or identity "")) identity))))

(defun reomacs/dailies--deduplicate-reference-candidates (candidates)
  "Deduplicate CANDIDATES by URL while preserving order."
  (let ((seen (make-hash-table :test 'equal)) result)
    (dolist (candidate candidates (nreverse result))
      (unless (gethash (plist-get candidate :url) seen)
        (puthash (plist-get candidate :url) t seen)
        (push candidate result)))))

(defun reomacs/dailies--ordered-completion-table (state)
  "Return a dynamic completion table backed by reference STATE."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action
       action
       (mapcar (lambda (candidate) (plist-get candidate :display))
               (plist-get state :candidates))
       string predicate))))

;;;###autoload
(defun reomacs/dailies-insert-reference (prompt)
  "Insert a recent forge reference using completion.
Without a prefix argument, query `reomacs/dailies-forge'.  With prefix
argument PROMPT, choose the current forge, both forges, a custom Azure
project, or GitHub.  Prompt for a repository, preselecting
`reomacs/dailies-default-repository'.  Local references appear immediately;
remote results stream into the active completion UI.  Azure tickets include
recent historical involvement and items linked to authored or reviewed PRs."
  (interactive "P")
  (unless (and (derived-mode-p 'org-mode)
               (reomacs/dailies--buffer-p))
    (user-error "This command requires the configured dailies buffer"))
  (let* ((sources
          (reomacs/dailies--prompt-reference-repository
           (reomacs/dailies--reference-sources prompt)))
         (local (reomacs/dailies--recent-reference-candidates sources))
         (requests (reomacs/dailies--reference-requests sources))
         (state (list :local local :remote nil :candidates local
                      :errors nil :pool nil :minibuffer nil :closed nil))
         choice)
    (unwind-protect
        (setq choice
              (minibuffer-with-setup-hook
                  (lambda ()
                    (plist-put state :minibuffer (current-buffer))
                    (reomacs/dailies--start-reference-fetch requests state))
                (completing-read
                 "Insert reference: "
                 (reomacs/dailies--ordered-completion-table state)
                 nil t nil 'reomacs/dailies-reference-history)))
      (plist-put state :closed t)
      (when-let* ((pool (plist-get state :pool)))
        (reomacs/dailies--request-pool-close pool)))
    (dolist (error (nreverse (plist-get state :errors)))
      (message "Dailies reference fetch error: %s" error))
    (let ((candidate
           (seq-find (lambda (item)
                       (equal choice (plist-get item :display)))
                     (plist-get state :candidates))))
      (unless candidate
        (user-error "Reference disappeared before insertion"))
      (insert (plist-get candidate :link))
      (message "Inserted forge reference%s"
               (let ((errors (length (plist-get state :errors))))
                 (if (zerop errors)
                     ""
                   (format "; %d remote request%s failed"
                           errors (if (= errors 1) "" "s"))))))))

;; ----------------------------------------------------------------------
;; Duration annotations

(defconst reomacs/dailies--duration-re
  (concat "{\\s-*"
          "\\(?:\\([0-9]+\\(?:\\.[0-9]+\\)?\\)h\\)?\\s-*"
          "\\(?:\\([0-9]+\\)m\\)?\\s-*"
          "\\(?:@\\(\\(?:[01]?[0-9]\\|2[0-3]\\):[0-5][0-9]\\)\\)?\\s-*"
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
                     (string-match reomacs/dailies--trailing-duration-re line)
                     (or (match-string 1 line) (match-string 2 line)))
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

(defface reomacs/dailies-duration-face
  '((t (:inherit org-tag :height 0.9 :weight semi-bold
                 :box (:line-width (1 . -1)))))
  "Face for concealed task duration chips."
  :group 'reo101)

(defvar-local reomacs/dailies--week-status-overlays nil
  "Dynamic week/day status overlays in the current dailies buffer.")

(defvar-local reomacs/dailies--duration-overlays nil
  "Concealed task duration overlays in the current dailies buffer.")

(defvar-local reomacs/dailies--revealed-duration-overlay nil
  "Duration overlay currently revealed for editing.")

(defun reomacs/dailies--duration-chip (hours minutes start complete)
  "Return a pretty duration chip for HOURS, MINUTES, START and COMPLETE."
  (let ((text (string-join
               (delq nil (list (and hours (concat hours "h"))
                               (and minutes (concat minutes "m"))))
               " ")))
    (when start
      (setq text (concat text (unless (string-empty-p text) " · ") start)))
    (when complete
      (setq text (concat text (unless (string-empty-p text) " ") "✓")))
    (propertize (format " 󰔛 %s " text)
                'face 'reomacs/dailies-duration-face)))

(defun reomacs/dailies--duration-overlay-at-point ()
  "Return the concealed duration overlay containing point, if any."
  (seq-find (lambda (overlay)
              (and (overlay-buffer overlay)
                   (<= (overlay-start overlay) (point))
                   (< (point) (overlay-end overlay))))
            reomacs/dailies--duration-overlays))

(defun reomacs/dailies--reveal-duration-at-point ()
  "Reveal the duration source under point and conceal the previous one."
  (let ((current (reomacs/dailies--duration-overlay-at-point)))
    (unless (eq current reomacs/dailies--revealed-duration-overlay)
      (when (and reomacs/dailies--revealed-duration-overlay
                 (overlay-buffer reomacs/dailies--revealed-duration-overlay))
        (overlay-put
         reomacs/dailies--revealed-duration-overlay 'display
         (overlay-get reomacs/dailies--revealed-duration-overlay
                      'reomacs/dailies-duration-display)))
      (setq reomacs/dailies--revealed-duration-overlay current)
      (when current
        (overlay-put current 'display nil)))))

(defun reomacs/dailies--refresh-duration-overlays ()
  "Rebuild concealed duration chips for task bullets."
  (mapc #'delete-overlay reomacs/dailies--duration-overlays)
  (setq reomacs/dailies--duration-overlays nil
        reomacs/dailies--revealed-duration-overlay nil)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward reomacs/dailies--trailing-duration-re nil t)
        (let ((beg (save-excursion
                     (goto-char (match-beginning 0))
                     (skip-chars-forward " \t" (match-end 0))
                     (point)))
              (end (match-end 0))
              (hours (match-string-no-properties 1))
              (minutes (match-string-no-properties 2))
              (start (match-string-no-properties 3))
              (complete (match-string-no-properties 4))
              (source (string-trim
                       (match-string-no-properties 0))))
          (when (and (or hours minutes)
                     (save-excursion
                       (goto-char beg)
                       (beginning-of-line)
                       (looking-at-p "[ \t]*\\+[ \t]+")))
            (let* ((display (reomacs/dailies--duration-chip
                             hours minutes start complete))
                   (overlay (make-overlay beg end)))
              (overlay-put overlay 'display display)
              (overlay-put overlay 'reomacs/dailies-duration-display display)
              (overlay-put overlay 'help-echo
                           (format "%s; move point here to edit" source))
              (overlay-put overlay 'evaporate t)
              (push overlay reomacs/dailies--duration-overlays)))))))
  (setq reomacs/dailies--duration-overlays
        (nreverse reomacs/dailies--duration-overlays))
  (reomacs/dailies--reveal-duration-at-point))

(defvar-local reomacs/dailies--hours-status-dirty nil
  "Non-nil when hour overlays need refreshing after the current command.")

(defvar-local reomacs/dailies--text-before-change nil
  "Text captured before a possible duration or heading change.")

(defun reomacs/dailies--capture-text-before-change (beg end)
  "Capture text between BEG and END before a dailies edit."
  (setq reomacs/dailies--text-before-change
        (buffer-substring-no-properties beg end)))

(defun reomacs/dailies--mark-hours-status-dirty (beg end _old-length)
  "Mark hour displays stale when the change from BEG to END affects them."
  (let* ((context
          (save-excursion
            (goto-char beg)
            (let ((line-beg (line-beginning-position)))
              (goto-char end)
              (buffer-substring-no-properties
               line-beg (line-end-position)))))
         (before (or reomacs/dailies--text-before-change ""))
         (heading-re "\\(?:\\`\\|\n\\)[ \t]*\\*+ "))
    (when (or (string-match-p "[{}\n]" before)
              (string-match-p "[{}\n]" context)
              (string-match-p heading-re before)
              (string-match-p heading-re context))
      (setq reomacs/dailies--hours-status-dirty t))
    (setq reomacs/dailies--text-before-change nil)))

(defun reomacs/dailies--refresh-hours-status-if-dirty ()
  "Refresh stale hour displays and reveal the duration under point."
  (when reomacs/dailies--hours-status-dirty
    (reomacs/dailies-refresh-week-status))
  (reomacs/dailies--reveal-duration-at-point))

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
  "Refresh task duration chips and hours after week and date headings."
  (interactive)
  (setq reomacs/dailies--hours-status-dirty nil)
  (reomacs/dailies--refresh-duration-overlays)
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
  "Configure duration chips and week/day status after local variables load."
  (if (and (derived-mode-p 'org-mode)
           (reomacs/dailies--buffer-p))
      (progn
        (add-hook 'before-change-functions
                  #'reomacs/dailies--capture-text-before-change nil t)
        (add-hook 'after-change-functions
                  #'reomacs/dailies--mark-hours-status-dirty nil t)
        (add-hook 'post-command-hook
                  #'reomacs/dailies--refresh-hours-status-if-dirty nil t)
        (reomacs/dailies-refresh-week-status))
    (remove-hook 'before-change-functions
                 #'reomacs/dailies--capture-text-before-change t)
    (remove-hook 'after-change-functions
                 #'reomacs/dailies--mark-hours-status-dirty t)
    (remove-hook 'post-command-hook
                 #'reomacs/dailies--refresh-hours-status-if-dirty t)
    (mapc #'delete-overlay reomacs/dailies--week-status-overlays)
    (mapc #'delete-overlay reomacs/dailies--duration-overlays)
    (setq reomacs/dailies--week-status-overlays nil
          reomacs/dailies--duration-overlays nil
          reomacs/dailies--revealed-duration-overlay nil
          reomacs/dailies--text-before-change nil)))

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
