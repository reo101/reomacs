;;; reomacs-dailies-test.el --- Tests for work dailies -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'reomacs-dailies)

(defmacro reomacs-dailies-test--with-org (text &rest body)
  "Create a temporary Org buffer containing TEXT, then run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (goto-char (point-min))
     ,@body))

(ert-deftest reomacs-dailies-date-navigation-ignores-org-metadata ()
  (reomacs-dailies-test--with-org
      "* Week 2026-W36\n** DONE [#A] <2026-08-31 Mon> :work:\n+ Task {2h}\n"
    (search-forward "Task")
    (should (equal (reomacs/dailies--goto-containing-date-heading)
                   "2026-08-31"))
    (should (= (org-current-level) 2))))

(ert-deftest reomacs-dailies-sums-supported-duration-forms ()
  (reomacs-dailies-test--with-org
      (concat "+ One {2h}\n"
              "+ Two {45m @13:00}\n"
              "+ Three {1.5h✓}\n"
              "+ Estimate {1h}, actual {2h}\n"
              "+ No duration\n")
    (should (= (reomacs/dailies--sum-duration-minutes
                (point-min) (point-max))
               375))))

(ert-deftest reomacs-dailies-canonicalizes-repository-identities ()
  (dolist (case '(("owner/repo.with.dot" . "owner/repo.with.dot")
                  ("git@github.com:owner/repo.git" . "owner/repo")
                  ("https://user@dev.azure.com/org/project/_git/repo"
                   . "org/project/repo")
                  ("dev.azure.com/org/project/_git/repo"
                   . "org/project/repo")))
    (should (equal (reomacs/dailies--canonical-repo-id (car case))
                   (cdr case))))
  (should (equal (reomacs/dailies--az-org-name
                  "https://dev.azure.com/baringa/")
                 "baringa")))

(ert-deftest reomacs-dailies-safe-locals-reject-code-and-bad-values ()
  (should (safe-local-variable-p
           'reomacs/dailies-repo-aliases '(("owner/repo" . "Name"))))
  (should-not (safe-local-variable-p
               'reomacs/dailies-repo-aliases
               '(("owner/repo" . (lambda () (shell-command "false"))))))
  (should (safe-local-variable-p 'reomacs/dailies-forge 'azure))
  (should-not (safe-local-variable-p
               'reomacs/dailies-forge 'shell-command)))

(ert-deftest reomacs-dailies-renders-day-and-week-status-overlays ()
  (reomacs-dailies-test--with-org
      (concat "* Week 2026-W36\n"
              "** DONE <2026-08-31 Mon>\n"
              "+ One {6.5h}\n"
              "** TODO <2026-09-01 Tue>\n"
              "+ Two {2h}\n")
    (setq-local reomacs/dailies-daily-target-hours 8)
    (reomacs/dailies-refresh-week-status)
    (let ((texts (mapcar (lambda (overlay)
                           (substring-no-properties
                            (overlay-get overlay 'after-string)))
                         reomacs/dailies--week-status-overlays)))
      (should (= (length texts) 3))
      (should (member " — 6.5/8h ✓" texts))
      (should (member " — 2/8h" texts))
      (should (member " — 8.5/40h" texts)))))

(ert-deftest reomacs-dailies-creates-flat-next-workday ()
  (reomacs-dailies-test--with-org
      "* Week 2099-W01\n** DONE <2099-01-01 Thu>\n+ Existing {8h}\n"
    (let ((buffer (current-buffer)))
      (setq-local reomacs/dailies-daily-target-hours 8)
      (cl-letf (((symbol-function 'reomacs/dailies--buffer)
                 (lambda () buffer))
                ((symbol-function 'reomacs/dailies-refresh-week-status)
                 #'ignore)
                ((symbol-function 'switch-to-buffer)
                 #'ignore))
        (reomacs/dailies-new-day))
      (should (string-match-p
               "\\*\\* TODO <2099-01-02 Fri>\n\n\\+ \n\\'"
               (buffer-string)))
      (should-not (string-match-p "\\*\\*\\* Today" (buffer-string))))))

(ert-deftest reomacs-dailies-copies-clean-markdown-with-optional-durations ()
  (reomacs-dailies-test--with-org
      (concat "* Week 2026-W36\n"
              "** TODO <2026-08-31 Mon>\n"
              "+ Meetings {30m}\n"
              "+ Review [[https://example.test/pr/1][PR 1]] using ~uom~ {2h}\n"
              "** TODO <2026-09-01 Tue>\n"
              "+ Wrong day {8h}\n")
    (search-forward "Review")
    (set-buffer-modified-p nil)
    (let (without with)
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &rest _) (setq without text))))
        (reomacs/dailies-day-to-markdown-clipboard nil))
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &rest _) (setq with text))))
        (reomacs/dailies-day-to-markdown-clipboard t))
      (should (equal without
                     (concat "- Meetings\n"
                             "- Review [PR 1](https://example.test/pr/1) using `uom`")))
      (should (equal with
                     (concat "- Meetings {30m}\n"
                             "- Review [PR 1](https://example.test/pr/1) using `uom` {2h}")))
      (should-not (buffer-modified-p)))))

(ert-deftest reomacs-dailies-resolves-the-exact-visiting-buffer ()
  (let* ((left-dir (make-temp-file "dailies-left-" t))
         (right-dir (make-temp-file "dailies-right-" t))
         (left-file (expand-file-name "dailies.org" left-dir))
         (right-file (expand-file-name "dailies.org" right-dir))
         left-buffer
         right-buffer)
    (unwind-protect
        (progn
          (write-region "" nil left-file nil 'silent)
          (write-region "" nil right-file nil 'silent)
          (setq left-buffer (find-file-noselect left-file)
                right-buffer (find-file-noselect right-file))
          (let ((reomacs/dailies-file right-file))
            (should (eq (reomacs/dailies--buffer) right-buffer))
            (should (reomacs/dailies--buffer-p right-buffer))
            (should-not (reomacs/dailies--buffer-p left-buffer))))
      (when (buffer-live-p left-buffer) (kill-buffer left-buffer))
      (when (buffer-live-p right-buffer) (kill-buffer right-buffer))
      (delete-directory left-dir t)
      (delete-directory right-dir t))))

(ert-deftest reomacs-dailies-converts-forge-timestamps-to-local-days ()
  (should (reomacs/dailies--date-on-day-p
           "2026-08-30T22:30:00Z" "2026-08-31" "Europe/Sofia"))
  (should-not (reomacs/dailies--date-on-day-p
               "2026-08-31T22:30:00Z" "2026-08-31" "Europe/Sofia"))
  (should (equal (reomacs/dailies--utc-search-range
                  "2026-08-31" "Europe/Sofia")
                 '("2026-08-30" "2026-08-31")))
  (should (equal (reomacs/dailies--utc-search-range
                  "2026-08-31" "UTC")
                 '("2026-08-31" "2026-08-31")))
  (should (equal (reomacs/dailies--date-add-days "2026-03-28" 2)
                 "2026-03-30"))
  (should-error (reomacs/dailies--date-add-days "2026-02-30" 1)))

(ert-deftest reomacs-dailies-emits-both-same-day-pr-events ()
  (let ((emitted nil)
        (pr '((url . "https://example.test/pr/1")
              (number . 1)
              (createdAt . "2026-08-31T08:00:00Z")
              (closedAt . "2026-08-31T16:00:00Z")
              (repository . ((name . "repo")
                             (nameWithOwner . "owner/repo"))))))
    (reomacs/dailies--process-prs
     (list :date "2026-08-31" :username "me" :aliases nil
           :emit (lambda (line) (push line emitted))
           :fetch (lambda (_command callback)
                    (funcall callback
                             "{\"actor\":\"me\",\"event\":\"merged\"}")))
     (list pr))
    (should (equal (nreverse emitted)
                   '("+ Create [[https://example.test/pr/1][~repo~ PR 1]]"
                     "+ Merge [[https://example.test/pr/1][~repo~ PR 1]]")))))

(ert-deftest reomacs-dailies-github-fetches-are-bounded-and-paginated ()
  (let (commands)
    (cl-letf (((symbol-function 'reomacs/dailies--utc-search-range)
               (lambda (&rest _) '("2026-08-30" "2026-08-31"))))
      (reomacs/dailies--run-gh-fetch
       (list :date "2026-08-31" :username "me" :aliases nil
             :emit #'ignore
             :fetch (lambda (command _callback) (push command commands)))))
    (should (= (length commands) 4))
    (dolist (command (seq-filter
                      (lambda (command) (member "search" command)) commands))
      (should (member "--limit=1000" command))
      (should (member "--updated=2026-08-30..2026-08-31" command)))
    (let ((events-command
           (seq-find (lambda (command) (member "--slurp" command)) commands)))
      (should (member "--paginate" events-command)))))

(ert-deftest reomacs-dailies-paginates-azure-pull-requests ()
  (let ((calls 0)
        commands
        (page (json-encode (make-list 500 '((pullRequestId . 1))))))
    (cl-letf (((symbol-function 'reomacs/dailies--process-az-prs) #'ignore))
      (reomacs/dailies--fetch-azure-pr-page
       (list :azure-org "https://dev.azure.com/org"
             :fetch (lambda (command callback)
                      (push command commands)
                      (setq calls (1+ calls))
                      (funcall callback (if (= calls 1) page "[]"))))
       "project" 0))
    (should (= calls 2))
    (should (member "500" (car commands)))))

(defun reomacs-dailies-test--wait-for-process (process)
  "Wait for PROCESS and its sentinel to finish."
  (while (process-live-p process)
    (accept-process-output process 0.1))
  (accept-process-output nil 0.05))

(ert-deftest reomacs-dailies-process-failure-finalizes-placeholder ()
  (reomacs-dailies-test--with-org
      (concat "+ " reomacs/dailies--placeholder "\n")
    (let* ((marker (copy-marker (point-min) t))
           (runner (reomacs/dailies--make-runner "2026-08-31" marker))
           (process (funcall (plist-get runner :fetch)
                             '("false") #'ignore)))
      (reomacs-dailies-test--wait-for-process process)
      (should (zerop (gethash :pending (plist-get runner :state))))
      (should-not (marker-buffer marker))
      (should (string-match-p
               "^\\+ ERROR: github interactions, 1 request failed$"
               (string-trim (buffer-string)))))))

(ert-deftest reomacs-dailies-callback-failure-finalizes-placeholder ()
  (reomacs-dailies-test--with-org
      (concat "+ " reomacs/dailies--placeholder "\n")
    (let* ((marker (copy-marker (point-min) t))
           (runner (reomacs/dailies--make-runner "2026-08-31" marker))
           (process
            (funcall (plist-get runner :fetch) '("printf" "{")
                     #'reomacs/dailies--json)))
      (reomacs-dailies-test--wait-for-process process)
      (should (zerop (gethash :pending (plist-get runner :state))))
      (should-not (marker-buffer marker))
      (should (string-match-p "^\\+ ERROR:" (buffer-string))))))

(ert-deftest reomacs-dailies-has-no-obsolete-submission-or-layout-paths ()
  (dolist (symbol '(reomacs/dailies-submit-timesheet
                    reomacs/dailies--dv-request
                    reomacs/dailies--dataverse-token
                    reomacs/dailies-insert-interactions-async
                    reomacs/dailies--fetch-interactions-async
                    reomacs/dailies--make-sync-runner
                    reomacs/dailies--make-async-runner
                    reomacs/dailies--gh-async
                    reomacs/dailies--yesterday-label))
    (should-not (fboundp symbol)))
  (dolist (symbol '(reomacs/dailies-flat-layout
                    reomacs/dailies-buffer-predicate
                    reomacs/dailies-event-handlers))
    (should-not (boundp symbol))))

;;; reomacs-dailies-test.el ends here
