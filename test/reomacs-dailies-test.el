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
              "+ Empty {}\n"
              "+ Invalid time {30m @99:99}\n"
              "*** TODO Heading task {2h}\n"
              "** Day <2026-08-31 Mon> {9h}\n"
              "+ No duration\n")
    (should (= (reomacs/dailies--sum-duration-minutes
                (point-min) (point-max))
               495))))

(ert-deftest reomacs-dailies-conceals-and-reveals-task-durations ()
  (reomacs-dailies-test--with-org
      (concat "+ One {2h}\n"
              "+ Two {45m @13:00}\n"
              "+ Three {1.5h✓}\n"
              "+ Empty {}\n"
              "+ Invalid time {30m @99:99}\n"
              "*** TODO Heading task {1h}\n"
              "Not a task {9h}\n")
    (set-buffer-modified-p nil)
    (reomacs/dailies-refresh-week-status)
    (should (equal
             (mapcar (lambda (overlay)
                       (substring-no-properties (overlay-get overlay 'display)))
                     reomacs/dailies--duration-overlays)
             '(" 󰔛 2h " " 󰔛 45m · 13:00 " " 󰔛 1.5h ✓ " " 󰔛 1h ")))
    (let ((overlay (cadr reomacs/dailies--duration-overlays)))
      (goto-char (overlay-start overlay))
      (reomacs/dailies--reveal-duration-at-point)
      (should-not (overlay-get overlay 'display))
      (should (equal (buffer-substring-no-properties
                      (overlay-start overlay) (overlay-end overlay))
                     "{45m @13:00}"))
      (goto-char (point-min))
      (reomacs/dailies--reveal-duration-at-point)
      (should (equal (substring-no-properties (overlay-get overlay 'display))
                     " 󰔛 45m · 13:00 ")))
    (should-not (buffer-modified-p))))

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
               'reomacs/dailies-forge 'shell-command))
  (should (safe-local-variable-p 'reomacs/dailies-azure-project "plcs"))
  (should-not (safe-local-variable-p
               'reomacs/dailies-azure-project '(shell-command "false")))
  (should (safe-local-variable-p
           'reomacs/dailies-default-repository "org/project/repo"))
  (should-not (safe-local-variable-p
               'reomacs/dailies-default-repository '(shell-command "false")))
  (should (safe-local-variable-p
           'reomacs/dailies-markdown-keep-durations nil))
  (should (safe-local-variable-p
           'reomacs/dailies-markdown-remove-links t))
  (should (safe-local-variable-p
           'reomacs/dailies-markdown-bullet "-"))
  (should-not (safe-local-variable-p
               'reomacs/dailies-markdown-bullet "#"))
  (should (safe-local-variable-p
           'reomacs/dailies-markdown-link-markup "plain"))
  (should-not (safe-local-variable-p
               'reomacs/dailies-markdown-link-markup "html")))

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

(ert-deftest reomacs-dailies-date-range-target-scales-by-inclusive-days ()
  (reomacs-dailies-test--with-org
      (concat "* Week 2026-W39\n"
              "** <2026-09-23 Wed>--<2026-09-25 Fri>\n"
              "+ Conference {32h}\n")
    (setq-local reomacs/dailies-daily-target-hours 8)
    (reomacs/dailies-refresh-week-status)
    (let ((texts (mapcar (lambda (overlay)
                           (substring-no-properties
                            (overlay-get overlay 'after-string)))
                         reomacs/dailies--week-status-overlays)))
      (should (member " — 32/24h" texts))
      (should (member " — 32/40h" texts)))))

(ert-deftest reomacs-dailies-hours-overlays-survive-folding ()
  (reomacs-dailies-test--with-org
      (concat "* Week 2026-W36\n"
              "** TODO <2026-08-31 Mon>\n"
              "+ One {6.5h}\n")
    (setq-local reomacs/dailies-daily-target-hours 8)
    (reomacs/dailies-refresh-week-status)
    (let ((overlay (nth 1 reomacs/dailies--week-status-overlays)))
      (should (= (overlay-start overlay) (1- (overlay-end overlay))))
      (goto-char (overlay-end overlay))
      (org-fold-hide-subtree)
      ;; The fold covers the line-end position but not the heading text,
      ;; so the overlay must rest entirely on visible characters.
      (should (get-char-property (overlay-end overlay) 'invisible))
      (should-not (get-char-property (overlay-start overlay) 'invisible)))))

(ert-deftest reomacs-dailies-only-dirties-hours-for-relevant-edits ()
  (reomacs-dailies-test--with-org
      "** TODO <2026-08-31 Mon>\n+ Ordinary task\n"
    (search-forward "Ordinary")
    (let ((beg (point)))
      (reomacs/dailies--capture-text-before-change beg beg)
      (insert " text")
      (reomacs/dailies--mark-hours-status-dirty beg (point) 0)
      (should-not reomacs/dailies--hours-status-dirty))
    (end-of-line)
    (let ((beg (point)))
      (reomacs/dailies--capture-text-before-change beg beg)
      (insert " {1h}")
      (reomacs/dailies--mark-hours-status-dirty beg (point) 0)
      (should reomacs/dailies--hours-status-dirty))
    (setq reomacs/dailies--hours-status-dirty nil)
    (goto-char (point-min))
    (let ((beg (point))
          (end (line-end-position)))
      (reomacs/dailies--capture-text-before-change beg end)
      (delete-region beg end)
      (reomacs/dailies--mark-hours-status-dirty beg beg (- end beg))
      (should reomacs/dailies--hours-status-dirty))))

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
               "\\*\\* TODO <2099-01-02 Fri>\n\n\\*\\*\\* TODO "
               (buffer-string)))
      (should-not (string-match-p "\\*\\*\\* Today" (buffer-string))))))

(ert-deftest reomacs-dailies-new-day-uses-greatest-date-not-file-order ()
  (reomacs-dailies-test--with-org
      (concat "* Week 2099-W01\n"
              "** DONE <2099-01-02 Fri>\n+ Later\n"
              "** DONE <2099-01-01 Thu>\n+ Earlier\n")
    (let ((buffer (current-buffer)))
      (cl-letf (((symbol-function 'reomacs/dailies--buffer)
                 (lambda () buffer))
                ((symbol-function 'reomacs/dailies-refresh-week-status)
                 #'ignore)
                ((symbol-function 'switch-to-buffer) #'ignore))
        (reomacs/dailies-new-day))
      (should (string-match-p "\\*\\* TODO <2099-01-05 Mon>"
                              (buffer-string)))
      (should-not (string-match-p "\\*\\* TODO <2099-01-02 Fri>"
                                  (buffer-string))))))

(ert-deftest reomacs-dailies-opens-and-focuses-buffer ()
  (let ((buffer (generate-new-buffer " *dailies-test*"))
        focused)
    (unwind-protect
        (cl-letf (((symbol-function 'reomacs/dailies--buffer)
                   (lambda () buffer))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (target) (setq focused target))))
          (reomacs/dailies-open)
          (should (eq focused buffer)))
      (kill-buffer buffer))))

(ert-deftest reomacs-dailies-goto-today-jumps-to-existing-entry ()
  (let ((stamp (format-time-string "<%Y-%m-%d %a>"))
        (today (format-time-string "%Y-%m-%d")))
    (reomacs-dailies-test--with-org
        (concat "* Week 2026-W36\n"
                "** TODO " stamp "\n+ Morning\n"
                "** TODO <2099-01-04 Thu>\n+ Later\n")
      (let ((buffer (current-buffer)))
        (cl-letf (((symbol-function 'reomacs/dailies--buffer)
                   (lambda () buffer))
                  ((symbol-function 'switch-to-buffer) #'ignore)
                  ((symbol-function 'recenter) #'ignore))
          (reomacs/dailies-goto-today))
        (should (equal (reomacs/dailies--date-at-heading) today))
        (should (= (how-many "^\\*\\* TODO" (point-min) (point-max))
                   2))))))

(ert-deftest reomacs-dailies-goto-today-creates-missing-entry-in-order ()
  (reomacs-dailies-test--with-org
      (concat "* Week 2099-W01\n"
              "** DONE <2099-01-01 Thu>\n+ Old\n"
              "** DONE <2099-01-05 Mon>\n+ Future\n")
    (let ((buffer (current-buffer))
          (today (format-time-string "%Y-%m-%d")))
      (cl-letf (((symbol-function 'reomacs/dailies--buffer)
                 (lambda () buffer))
                ((symbol-function 'switch-to-buffer) #'ignore)
                ((symbol-function 'recenter) #'ignore))
        (reomacs/dailies-goto-today))
      (should (equal (reomacs/dailies--date-at-heading) today))
      (should (< (reomacs/dailies--find-date-heading today)
                 (reomacs/dailies--find-date-heading "2099-01-01"))))))

(ert-deftest reomacs-dailies-flattens-task-heading-faces ()
  (reomacs-dailies-test--with-org "* Week 2026-W36\n"
    (cl-letf (((symbol-function 'reomacs/dailies--buffer-p)
               (lambda (&optional _) t)))
      (reomacs/dailies--configure-week-status)
      (should (assq 'org-level-3 face-remapping-alist))
      (should (memq 'default
                    (cdr (assq 'org-level-3 face-remapping-alist))))
      (should (null reomacs/dailies--week-status-overlays)))
    ;; Non-dailies teardown removes the remap
    (reomacs/dailies--configure-week-status)
    (should-not (assq 'org-level-3 face-remapping-alist))))

(ert-deftest reomacs-dailies-new-day-respects-bullet-style ()
  (reomacs-dailies-test--with-org
      "* Week 2099-W01\n** DONE <2099-01-01 Thu>\n+ Existing {8h}\n"
    (let ((buffer (current-buffer))
          (reomacs/dailies-task-style 'bullet))
      (cl-letf (((symbol-function 'reomacs/dailies--buffer)
                 (lambda () buffer))
                ((symbol-function 'reomacs/dailies-refresh-week-status)
                 #'ignore)
                ((symbol-function 'switch-to-buffer) #'ignore))
        (reomacs/dailies-new-day))
      (should (string-match-p
               "\\*\\* TODO <2099-01-02 Fri>\n\n\\+ \n"
               (buffer-string)))
      (should-not (string-match-p "\\*\\*\\* " (buffer-string))))))

(ert-deftest reomacs-dailies-new-day-date-skips-weekends ()
  ;; Friday pending → Friday; Saturday with pending Friday → Monday
  (should (equal (reomacs/dailies--next-new-day-date "2026-09-04" "2026-09-04")
                 "2026-09-07"))
  (should (equal (reomacs/dailies--next-new-day-date "2026-09-03" "2026-09-04")
                 "2026-09-04"))
  (should (equal (reomacs/dailies--next-new-day-date "2026-09-03" "2026-09-05")
                 "2026-09-07"))
  (should (equal (reomacs/dailies--next-new-day-date "2026-09-04" "2026-09-05")
                 "2026-09-07"))
  ;; Latest entry in the future chains from it
  (should (equal (reomacs/dailies--next-new-day-date "2026-09-07" "2026-09-05")
                 "2026-09-08")))

(ert-deftest reomacs-dailies-meta-return-inserts-compact-headings ()
  (reomacs-dailies-test--with-org
      (concat "* Week 2026-W36\n\n"
              "** TODO <2026-08-31 Mon>\n\n"
              "*** TODO First {1h}\n")
    (cl-letf (((symbol-function 'reomacs/dailies--buffer-p)
               (lambda (&optional _) t)))
      (reomacs/dailies--configure-week-status)
      (goto-char (point-max))
      (org-meta-return)
      (insert "Second")
      (should (string-match-p
               "First {1h}\n\\*\\*\\* Second"
               (buffer-string)))))
  ;; Non-dailies teardown restores default separation behavior
  (reomacs-dailies-test--with-org "* Heading\n"
    (reomacs/dailies--configure-week-status)
    (should (equal org-blank-before-new-entry
                   '((heading . auto) (plain-list-item . auto))))))

(ert-deftest reomacs-dailies-copies-clean-markdown-with-optional-durations ()
  (reomacs-dailies-test--with-org
      (concat "* Week 2026-W36\n"
              "** TODO <2026-08-31 Mon>\n"
              "+ Meetings {30m}\n\n"
              "+ Review [[https://example.test/pr/1][PR 1]] using ~uom~ {2h}\n"
              "** TODO <2026-09-01 Tue>\n"
              "+ Wrong day {8h}\n")
    (search-forward "Review")
    (set-buffer-modified-p nil)
    (let (without with without-links styled)
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &rest _) (setq without text))))
        (reomacs/dailies-day-to-markdown-clipboard nil))
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &rest _) (setq with text))))
        (reomacs/dailies-day-to-markdown-clipboard t))
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &rest _) (setq without-links text))))
        (reomacs/dailies-day-to-markdown-clipboard nil t))
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &rest _) (setq styled text))))
        (reomacs/dailies-day-to-markdown-clipboard nil t "+" "code"))
      (should (equal without
                     (concat "- Meetings\n"
                             "- Review [PR 1](https://example.test/pr/1) using `uom`")))
      (should (equal with
                     (concat "- Meetings {30m}\n"
                             "- Review [PR 1](https://example.test/pr/1) using `uom` {2h}")))
      (should (equal without-links
                     (concat "- Meetings\n"
                             "- Review PR 1 using `uom`")))
      (should (equal styled
                     (concat "+ Meetings\n"
                             "+ Review `PR 1` using `uom`")))
      (should (equal (reomacs/dailies--remove-org-links
                      "[[https://example.test][PR 1]]" "emphasis")
                     "*PR 1*"))
      (should-not (buffer-modified-p)))))

(ert-deftest reomacs-dailies-copies-heading-tasks-as-compact-bullets ()
  (reomacs-dailies-test--with-org
      (concat "* Week 2026-W36\n"
              "** TODO <2026-08-31 Mon>\n"
              "*** TODO First task {1h}\n\n"
              "*** DONE Second task\n")
    (search-forward "First task")
    (set-buffer-modified-p nil)
    (let (copied)
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &rest _) (setq copied text))))
        (reomacs/dailies-day-to-markdown-clipboard nil nil "+" "plain"))
      (should (equal copied
                     "+ First task\n+ Second task"))
      (should-not (buffer-modified-p)))))

(ert-deftest reomacs-dailies-markdown-uses-local-defaults ()
  (reomacs-dailies-test--with-org
      (concat "* Week 2026-W36\n"
              "** TODO <2026-08-31 Mon>\n"
              "*** TODO Review [[https://example.test][PR 1]] {2h}\n")
    (search-forward "Review")
    (setq-local reomacs/dailies-markdown-keep-durations nil
                reomacs/dailies-markdown-remove-links t
                reomacs/dailies-markdown-bullet "+"
                reomacs/dailies-markdown-link-markup "code")
    (set-buffer-modified-p nil)
    (let (copied)
      (let ((current-prefix-arg nil))
        (cl-letf (((symbol-function 'kill-new)
                   (lambda (text &rest _) (setq copied text))))
          (call-interactively #'reomacs/dailies-day-to-markdown-clipboard)))
      (should (equal copied "+ Review `PR 1`"))
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

(ert-deftest reomacs-dailies-scans-every-azure-pr-open-on-target-day ()
  (let ((open-pr '((creationDate . "2026-08-01T08:00:00Z")
                   (closedDate . "2026-09-01T08:00:00Z")
                   (createdBy . ((uniqueName . "someone-else")))
                   (reviewers . []))))
    (should (reomacs/dailies--az-review-candidate-p
             open-pr "me" "2026-08-31"))
    (should-not
     (reomacs/dailies--az-review-candidate-p
      '((creationDate . "2026-09-01T08:00:00Z")
        (createdBy . ((uniqueName . "someone-else"))))
      "me" "2026-08-31"))
    (should-not
     (reomacs/dailies--az-review-candidate-p
      '((creationDate . "2026-08-01T08:00:00Z")
        (closedDate . "2026-08-30T08:00:00Z")
        (createdBy . ((uniqueName . "someone-else"))))
      "me" "2026-08-31"))
    (should-not
     (reomacs/dailies--az-review-candidate-p
      '((creationDate . "2026-08-01T08:00:00Z")
        (createdBy . ((uniqueName . "me"))))
      "me" "2026-08-31"))))

(ert-deftest reomacs-dailies-prioritizes-recent-local-references ()
  (reomacs-dailies-test--with-org
      (concat
       "* Week 2026-W35\n"
       "** DONE <2026-08-28 Fri>\n"
       "+ [[https://dev.azure.com/org/plcs/_git/repo/pullrequest/1][Old Azure]]\n"
       "+ [[https://github.com/owner/repo/issues/2][GitHub]]\n"
       "* Week 2026-W36\n"
       "** TODO <2026-08-31 Mon>\n"
       "+ [[https://dev.azure.com/org/plcs/_workitems/edit/3][New Azure]]\n"
       "+ [[https://dev.azure.com/org/plcs/_git/other/pullrequest/4][Other repo]]\n")
    (should
     (equal
      (mapcar (lambda (candidate) (plist-get candidate :display))
              (reomacs/dailies--recent-reference-candidates
               '((:forge azure :project "plcs" :repository "repo"))))
      '("Recent  New Azure" "Recent  Old Azure")))))

(ert-deftest reomacs-dailies-caches-local-reference-index-until-text-changes ()
  (reomacs-dailies-test--with-org
      (concat "* Week 2026-W36\n"
              "** TODO <2026-08-31 Mon>\n"
              "+ [[https://github.com/owner/repo/issues/1][One]]\n"
              "** TODO <2026-09-01 Tue>\n"
              "+ [[https://github.com/owner/repo/issues/2][Two]]\n")
    (search-forward "<2026-08-31")
    (org-back-to-heading)
    (org-narrow-to-subtree)
    (let ((first (reomacs/dailies--local-reference-index)))
      (should (= (length first) 2))
      (should (eq first (reomacs/dailies--local-reference-index)))
      (widen)
      (goto-char (point-max))
      (insert "+ [[https://github.com/owner/repo/issues/3][Three]]\n")
      (let ((second (reomacs/dailies--local-reference-index)))
        (should-not (eq first second))
        (should (= (length second) 3))))))

(ert-deftest reomacs-dailies-builds-reference-candidates-and-requests ()
  (let* ((reomacs/dailies-azure-org "https://dev.azure.com/org")
         (reomacs/dailies-repo-aliases '(("owner/repo" . "Repo")))
         (work-output
          (json-encode
           [((id . 42)
             (fields . ((System.Title . "Fix [units]")
                        (System.WorkItemType . "Task")
                        (System.ChangedDate . "2026-08-31T10:00:00Z"))))]))
         (github-output
          (json-encode
           [((number . 7)
             (isPullRequest . t)
             (url . "https://github.com/owner/repo/pull/7")
             (title . "Refactor")
             (updatedAt . "2026-08-31T11:00:00Z")
             (repository . ((name . "repo")
                            (nameWithOwner . "owner/repo"))))]))
         (pr-output
          (json-encode
           [((pullRequestId . 17)
             (createdBy . ((uniqueName . "me")))
             (reviewers . []))
            ((pullRequestId . 18)
             (createdBy . ((uniqueName . "someone-else")))
             (reviewers . []))]))
         (work
          (car (reomacs/dailies--parse-azure-work-reference-candidates
                work-output reomacs/dailies-azure-org "plcs")))
         (github
          (car (reomacs/dailies--parse-github-reference-candidates
                github-output reomacs/dailies-repo-aliases)))
         (requests
          (reomacs/dailies--reference-requests
           '((:forge azure :project "plcs" :repository "repo")
             (:forge github :repository "owner/repo")))))
    (should (equal (plist-get work :link)
                   "[[https://dev.azure.com/org/plcs/_workitems/edit/42][~plcs~ Task 42: Fix (units)]]"))
    (should (equal (plist-get github :link)
                   "[[https://github.com/owner/repo/pull/7][~Repo~ PR 7: Refactor]]"))
    (should (= (length requests) 3))
    (should (seq-some
             (lambda (request)
               (member "--include-prs" (plist-get request :command)))
             requests))
    (should (seq-some
             (lambda (request)
               (member "plcs" (plist-get request :command)))
             requests))
    (should (seq-some
             (lambda (request)
               (member "--repo=owner/repo" (plist-get request :command)))
             requests))
    (let* ((username-request
            (seq-find
             (lambda (request)
               (member "account" (plist-get request :command)))
             requests))
           (pr-request
            (car (funcall (plist-get username-request :followups) "me\n")))
           (linked-requests
            (funcall (plist-get pr-request :followups) pr-output)))
      (should (member "--repository" (plist-get pr-request :command)))
      (should (= (length linked-requests) 1))
      (should (equal (plist-get (car linked-requests) :command)
                     '("az" "repos" "pr" "work-item" "list"
                       "--org" "https://dev.azure.com/org"
                       "--id" "17" "-o" "json"))))
    (should (seq-some
             (lambda (request)
               (seq-some (lambda (argument)
                           (and (stringp argument)
                                (string-match-p
                                 "EVER \\[System.AssignedTo\\]" argument)))
                         (plist-get request :command)))
             requests))))

(ert-deftest reomacs-dailies-preselects-configured-reference-repository ()
  (let* ((reomacs/dailies-repo-aliases
          '(("baringa/plcs/coral-asset-rust-bess" . "CRB")))
         (reomacs/dailies-default-repository
          "baringa/plcs/coral-asset-rust-bess")
         (sources '((:forge azure :project "plcs")))
         (choices
          (reomacs/dailies--reference-repository-choices sources))
         (scoped
          (reomacs/dailies--apply-reference-repository
           sources reomacs/dailies-default-repository)))
    (should (equal (caar choices)
                   "CRB  baringa/plcs/coral-asset-rust-bess"))
    (should (equal (plist-get (car scoped) :repository)
                   "coral-asset-rust-bess"))
    (should (equal (cdar (last choices)) nil))))

(ert-deftest reomacs-dailies-reference-picker-shows-local-links-immediately ()
  (reomacs-dailies-test--with-org
      (concat "* Week 2026-W36\n"
              "** TODO <2026-08-31 Mon>\n"
              "+ [[https://dev.azure.com/org/plcs/_workitems/edit/42][Task 42]]\n"
              "+ ")
    (goto-char (point-max))
    (let ((reomacs/dailies-forge 'azure)
          (reomacs/dailies-azure-project "plcs"))
      (cl-letf (((symbol-function 'reomacs/dailies--buffer-p)
                 (lambda (&optional _) t))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (car (all-completions "" collection)))))
        (reomacs/dailies-insert-reference nil)))
    (should (string-suffix-p
             "+ [[https://dev.azure.com/org/plcs/_workitems/edit/42][Task 42]]"
             (buffer-string)))))

(ert-deftest reomacs-dailies-reference-table-streams-remote-results ()
  (let* ((local (list (list :url "local" :display "Recent local")))
         (remote (list (list :url "remote" :display "Remote result"
                             :updated "2026-08-31T10:00:00Z")))
         (state (list :local local :remote nil :candidates local
                      :errors nil :pool nil :minibuffer nil :closed nil))
         (table (reomacs/dailies--ordered-completion-table state)))
    (should (equal (all-completions "" table) '("Recent local")))
    (cl-letf (((symbol-function 'run-at-time) #'ignore))
      (reomacs/dailies--update-reference-state state remote nil))
    (should (equal (all-completions "" table)
                   '("Recent local" "Remote result")))))

(ert-deftest reomacs-dailies-request-pool-limits-concurrency ()
  (let ((reomacs/dailies-process-concurrency 2)
        pool callbacks
        (idle-count 0)
        errors)
    (setq pool
          (reomacs/dailies--make-request-pool
           (lambda (command message)
             (push (cons command message) errors))
           (lambda () (setq idle-count (1+ idle-count)))))
    (cl-letf (((symbol-function 'reomacs/dailies--run-process)
               (lambda (command callback)
                 (push (cons command callback) callbacks)
                 (make-symbol "process"))))
      (puthash :holding t pool)
      (dotimes (index 5)
        (reomacs/dailies--request-pool-submit
         pool (list (number-to-string index)) #'ignore))
      (puthash :holding nil pool)
      (reomacs/dailies--request-pool-pump pool)
      (should (= (gethash :active pool) 2))
      (should (= (gethash :pending pool) 5))
      (while (> (gethash :pending pool) 0)
        (let ((request (pop callbacks)))
          (should request)
          (funcall (cdr request) "" nil)
          (should (<= (gethash :active pool) 2))))
      (should (zerop (gethash :active pool)))
      (should (gethash :finished pool))
      (should (= idle-count 1))
      (should-not errors))))

(ert-deftest reomacs-dailies-request-pool-cancels-running-processes ()
  (let ((pool (reomacs/dailies--make-request-pool #'ignore #'ignore)))
    (reomacs/dailies--request-pool-submit pool '("sleep" "10") #'ignore)
    (let ((process (car (gethash :processes pool))))
      (should (process-live-p process))
      (reomacs/dailies--request-pool-close pool)
      (accept-process-output process 0.1)
      (should-not (process-live-p process))
      (should (gethash :closed pool)))))

(ert-deftest reomacs-dailies-reference-fetch-runs-followups ()
  (let ((state (list :local nil :remote nil :candidates nil
                     :errors nil :pool nil :minibuffer nil :closed nil))
        callbacks)
    (cl-letf (((symbol-function 'reomacs/dailies--run-process)
               (lambda (command callback)
                 (push (cons command callback) callbacks)
                 (make-symbol "process")))
              ((symbol-function 'run-at-time) #'ignore))
      (reomacs/dailies--start-reference-fetch
       (list
        (list :command '("parent")
              :parser (lambda (_)
                        (list (list :url "parent" :display "Parent")))
              :followups
              (lambda (_)
                (list
                 (list :command '("child")
                       :parser
                       (lambda (_)
                         (list (list :url "child" :display "Child"))))))))
       state)
      (should (= (gethash :pending (plist-get state :pool)) 1))
      (funcall (cdr (assoc '("parent") callbacks)) "parent" nil)
      (should (= (gethash :pending (plist-get state :pool)) 1))
      (funcall (cdr (assoc '("child") callbacks)) "child" nil)
      (should (zerop (gethash :pending (plist-get state :pool))))
      (should (equal (mapcar (lambda (candidate)
                              (plist-get candidate :display))
                            (plist-get state :candidates))
                     '("Child" "Parent"))))))

(ert-deftest reomacs-dailies-reference-refreshes-vertico-in-place ()
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (state nil)
          exhibited)
      (set (make-local-variable 'vertico--input) '("typed" . 5))
      (setq state (list :minibuffer buffer :closed nil
                        :pool nil :candidates '((:display "Recent"))))
      (cl-letf (((symbol-function 'active-minibuffer-window)
                 (lambda () 'window))
                ((symbol-function 'window-buffer)
                 (lambda (_) buffer))
                ((symbol-function 'vertico--exhibit)
                 (lambda () (setq exhibited t))))
        (reomacs/dailies--refresh-reference-minibuffer state))
      (should exhibited)
      (should-not vertico--input))))

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
                   '("Create [[https://example.test/pr/1][~repo~ PR 1]]"
                     "Merge [[https://example.test/pr/1][~repo~ PR 1]]")))))

(ert-deftest reomacs-dailies-emit-uses-configured-task-prefix ()
  (reomacs-dailies-test--with-org "+ Existing\n"
    (let ((marker (copy-marker (point-min) t))
          (reomacs/dailies-forge 'github))
      (let ((bullet-runner
             (let ((reomacs/dailies-task-style 'bullet))
               (reomacs/dailies--make-runner "2026-08-31" marker))))
        (funcall (plist-get bullet-runner :emit) "Create PR 1")
        (should (equal (gethash :results (plist-get bullet-runner :state))
                       '("+ Create PR 1"))))
      (let ((heading-runner
             (let ((reomacs/dailies-task-style 'heading))
               (reomacs/dailies--make-runner "2026-08-31" marker))))
        (funcall (plist-get heading-runner :emit) "Create PR 1")
        (should (equal (gethash :results (plist-get heading-runner :state))
                       '("*** Create PR 1")))))))

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

(ert-deftest reomacs-dailies-paginates-azure-projects ()
  (let ((calls 0)
        commands
        (page
         (json-encode
          (list (cons 'value (make-list 500 '((name . "project"))))))))
    (cl-letf (((symbol-function 'reomacs/dailies--fetch-azure-pr-page)
               #'ignore))
      (reomacs/dailies--fetch-azure-project-page
       (list :azure-org "https://dev.azure.com/org"
             :fetch (lambda (command callback)
                      (push command commands)
                      (setq calls (1+ calls))
                      (funcall callback (if (= calls 1) page "{\"value\":[]}"))))
       0))
    (should (= calls 2))
    (should (member "500" (car commands)))))

(ert-deftest reomacs-dailies-paginates-azure-reference-prs ()
  (let* ((request
          (reomacs/dailies--azure-pr-reference-request
           "https://dev.azure.com/org" "project" "repo" nil "me"))
         (page
          (json-encode
           (make-list
            100 '((pullRequestId . 1)
                  (createdBy . ((uniqueName . "someone-else")))
                  (reviewers . [])))))
         (followups (funcall (plist-get request :followups) page)))
    (should (= (length followups) 1))
    (should (equal (cadr (member "--skip"
                                 (plist-get (car followups) :command)))
                   "100"))))

(defun reomacs-dailies-test--wait-for-pool (pool)
  "Wait for every request in POOL to finish."
  (while (> (gethash :pending pool) 0)
    (accept-process-output nil 0.1))
  (accept-process-output nil 0.05))

(ert-deftest reomacs-dailies-process-failure-finalizes-placeholder ()
  (dolist (style '(bullet heading))
    (reomacs-dailies-test--with-org
        (concat (if (eq style 'bullet) "+ " "*** ")
                reomacs/dailies--placeholder "\n")
      (let* ((marker (copy-marker (point-min) t))
             (reomacs/dailies-task-style style)
             (runner (reomacs/dailies--make-runner "2026-08-31" marker)))
        (funcall (plist-get runner :fetch) '("false") #'ignore)
        (reomacs-dailies-test--wait-for-pool (plist-get runner :pool))
        (should (zerop (gethash :pending (plist-get runner :pool))))
        (should-not (marker-buffer marker))
        (should (string-match-p
                 "^\\(?:\\+ \\|\\*\\*\\* \\)ERROR: github interactions, 1 request failed$"
                 (string-trim (buffer-string))))))))

(ert-deftest reomacs-dailies-callback-failure-finalizes-placeholder ()
  (dolist (style '(bullet heading))
    (reomacs-dailies-test--with-org
        (concat (if (eq style 'bullet) "+ " "*** ")
                reomacs/dailies--placeholder "\n")
      (let* ((marker (copy-marker (point-min) t))
             (reomacs/dailies-task-style style)
             (runner (reomacs/dailies--make-runner "2026-08-31" marker)))
        (funcall (plist-get runner :fetch) '("printf" "{")
                 #'reomacs/dailies--json)
        (reomacs-dailies-test--wait-for-pool (plist-get runner :pool))
        (should (zerop (gethash :pending (plist-get runner :pool))))
        (should-not (marker-buffer marker))
        (should (string-match-p "ERROR:" (buffer-string)))))))

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
