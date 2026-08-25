;;; reomacs-treesit-rainbow-test.el --- Tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'reomacs-treesit-rainbow)

(defvar org-src-fontify-natively)

(defun reomacs-treesit-rainbow-test--face-p (position face)
  "Return non-nil when FACE applies at POSITION."
  (let ((value (get-text-property position 'face)))
    (memq face (if (listp value) value (list value)))))

(defun reomacs-treesit-rainbow-test--positions (character)
  "Return buffer positions containing CHARACTER."
  (save-excursion
    (goto-char (point-min))
    (let (positions)
      (while (search-forward (char-to-string character) nil t)
        (push (1- (point)) positions))
      (nreverse positions))))

(defmacro reomacs-treesit-rainbow-test--with-mode
    (language mode text &rest body)
  "Run BODY in a temporary LANGUAGE buffer using MODE and TEXT."
  (declare (indent 3))
  `(progn
     (skip-unless (treesit-language-available-p ,language))
     (with-temp-buffer
       (insert ,text)
       (funcall ,mode)
       (reomacs-treesit-rainbow-enable)
       (font-lock-ensure)
       ,@body)))

(ert-deftest reomacs-treesit-rainbow-queries-compile ()
  (let ((available 0))
    (dolist (entry reomacs-treesit-rainbow--language-specs)
      (when (treesit-language-available-p (car entry))
        (cl-incf available)
        (should (reomacs-treesit-rainbow--settings
                 (car entry) (cdr entry)))))
    (skip-unless (> available 0))))

(ert-deftest reomacs-treesit-rainbow-cpp-semantic-angles ()
  (require 'c-ts-mode)
  (reomacs-treesit-rainbow-test--with-mode
      'cpp #'c++-ts-mode
      "template <typename T> void f(Vec<Vec<T>> x) { if (x < y) {} }"
    (pcase-let ((`(,template ,outer ,inner ,comparison)
                 (reomacs-treesit-rainbow-test--positions ?<))
                (`(,template-close ,inner-close ,outer-close)
                 (reomacs-treesit-rainbow-test--positions ?>)))
      (should (reomacs-treesit-rainbow-test--face-p
               template 'rainbow-delimiters-depth-1-face))
      (should (reomacs-treesit-rainbow-test--face-p
               outer 'rainbow-delimiters-depth-2-face))
      (should (reomacs-treesit-rainbow-test--face-p
               inner 'rainbow-delimiters-depth-3-face))
      (should-not (reomacs-treesit-rainbow-test--face-p
                   comparison 'rainbow-delimiters-depth-1-face))
      (should (reomacs-treesit-rainbow-test--face-p
               template-close 'rainbow-delimiters-depth-1-face))
      (should (reomacs-treesit-rainbow-test--face-p
               inner-close 'rainbow-delimiters-depth-3-face))
      (should (reomacs-treesit-rainbow-test--face-p
               outer-close 'rainbow-delimiters-depth-2-face)))))

(ert-deftest reomacs-treesit-rainbow-rust-generics-and-closure ()
  (require 'rust-ts-mode)
  (reomacs-treesit-rainbow-test--with-mode
      'rust #'rust-ts-mode
      "fn f<T>(x: Vec<Vec<T>>) { call(|y| { y }); }"
    (pcase-let ((`(,parameters ,outer ,inner)
                 (reomacs-treesit-rainbow-test--positions ?<))
                (`(,parameters-close ,inner-close ,outer-close)
                 (reomacs-treesit-rainbow-test--positions ?>))
                (`(,open-pipe ,close-pipe)
                 (reomacs-treesit-rainbow-test--positions ?|)))
      (should (reomacs-treesit-rainbow-test--face-p
               parameters 'rainbow-delimiters-depth-1-face))
      (should (reomacs-treesit-rainbow-test--face-p
               outer 'rainbow-delimiters-depth-2-face))
      (should (reomacs-treesit-rainbow-test--face-p
               inner 'rainbow-delimiters-depth-3-face))
      (should (reomacs-treesit-rainbow-test--face-p
               parameters-close 'rainbow-delimiters-depth-1-face))
      (should (reomacs-treesit-rainbow-test--face-p
               inner-close 'rainbow-delimiters-depth-3-face))
      (should (reomacs-treesit-rainbow-test--face-p
               outer-close 'rainbow-delimiters-depth-2-face))
      (dolist (position (list open-pipe close-pipe))
        (should (reomacs-treesit-rainbow-test--face-p
                 position 'rainbow-delimiters-depth-3-face))))))

(ert-deftest reomacs-treesit-rainbow-tsx-skips-jsx-tags ()
  (require 'typescript-ts-mode)
  (reomacs-treesit-rainbow-test--with-mode
      'tsx #'tsx-ts-mode
      "const f = <T,>(x: T) => <div>{x}</div>;"
    (pcase-let ((`(,generic ,jsx-open ,jsx-close)
                 (reomacs-treesit-rainbow-test--positions ?<))
                (`(,generic-close . ,_)
                 (reomacs-treesit-rainbow-test--positions ?>)))
      (should (reomacs-treesit-rainbow-test--face-p
               generic 'rainbow-delimiters-depth-1-face))
      (should (reomacs-treesit-rainbow-test--face-p
               generic-close 'rainbow-delimiters-depth-1-face))
      (should-not (reomacs-treesit-rainbow-test--face-p
                   jsx-open 'rainbow-delimiters-depth-1-face))
      (should-not (reomacs-treesit-rainbow-test--face-p
                   jsx-close 'rainbow-delimiters-depth-1-face)))))

(ert-deftest reomacs-treesit-rainbow-syntax-fallback ()
  (require 'python)
  (reomacs-treesit-rainbow-test--with-mode
      'python #'python-ts-mode "f([x])"
    (should rainbow-delimiters-mode)
    (should-not reomacs-treesit-rainbow-mode)
    (should (reomacs-treesit-rainbow-test--face-p
             2 'rainbow-delimiters-depth-1-face))
    (should (reomacs-treesit-rainbow-test--face-p
             3 'rainbow-delimiters-depth-2-face))))

(ert-deftest reomacs-treesit-rainbow-elisp-extras ()
  (with-temp-buffer
    (insert "(foo :bar [\"x\"])")
    (emacs-lisp-mode)
    (reomacs-treesit-rainbow-enable)
    (font-lock-ensure)
    (should rainbow-delimiters-mode)
    (should reomacs-treesit-rainbow-elisp-mode)
    (should (reomacs-treesit-rainbow-test--face-p
             6 'rainbow-delimiters-depth-2-face))
    (dolist (position '(12 14))
      (should (reomacs-treesit-rainbow-test--face-p
               position 'rainbow-delimiters-depth-3-face)))))

(ert-deftest reomacs-treesit-rainbow-updates-after-edit ()
  (require 'c-ts-mode)
  (reomacs-treesit-rainbow-test--with-mode
      'cpp #'c++-ts-mode "Vec<T> x;"
    (goto-char 5)
    (insert "Vec<")
    (search-forward ">")
    (insert ">")
    (font-lock-ensure)
    (pcase-let ((`(,outer ,inner)
                 (reomacs-treesit-rainbow-test--positions ?<))
                (`(,inner-close ,outer-close)
                 (reomacs-treesit-rainbow-test--positions ?>)))
      (should (reomacs-treesit-rainbow-test--face-p
               outer 'rainbow-delimiters-depth-1-face))
      (should (reomacs-treesit-rainbow-test--face-p
               inner 'rainbow-delimiters-depth-2-face))
      (should (reomacs-treesit-rainbow-test--face-p
               inner-close 'rainbow-delimiters-depth-2-face))
      (should (reomacs-treesit-rainbow-test--face-p
               outer-close 'rainbow-delimiters-depth-1-face)))))

(ert-deftest reomacs-treesit-rainbow-restores-font-lock-state ()
  (require 'c-ts-mode)
  (reomacs-treesit-rainbow-test--with-mode
      'cpp #'c++-ts-mode "Vec<T> x;"
    (let ((original-settings reomacs-treesit-rainbow--original-settings)
          (original-features reomacs-treesit-rainbow--original-features))
      (should reomacs-treesit-rainbow-mode)
      (reomacs-treesit-rainbow-mode -1)
      (should (eq treesit-font-lock-settings original-settings))
      (should (eq treesit-font-lock-feature-list original-features)))))

(ert-deftest reomacs-treesit-rainbow-buffer-state-is-local ()
  (require 'c-ts-mode)
  (require 'rust-ts-mode)
  (skip-unless (and (treesit-language-available-p 'cpp)
                    (treesit-language-available-p 'rust)))
  (let ((cpp-buffer (generate-new-buffer " *rainbow-cpp*"))
        (rust-buffer (generate-new-buffer " *rainbow-rust*")))
    (unwind-protect
        (progn
          (with-current-buffer cpp-buffer
            (insert "Vec<T> x;")
            (c++-ts-mode)
            (reomacs-treesit-rainbow-enable)
            (font-lock-ensure))
          (with-current-buffer rust-buffer
            (insert "fn f<T>() {}")
            (rust-ts-mode)
            (reomacs-treesit-rainbow-enable)
            (font-lock-ensure)
            (reomacs-treesit-rainbow-mode -1))
          (with-current-buffer cpp-buffer
            (should reomacs-treesit-rainbow-mode)
            (should (reomacs-treesit-rainbow-test--face-p
                     4 'rainbow-delimiters-depth-1-face))))
      (kill-buffer cpp-buffer)
      (kill-buffer rust-buffer))))

(ert-deftest reomacs-treesit-rainbow-org-native-fontification ()
  (require 'org)
  (require 'c-ts-mode)
  (skip-unless (treesit-language-available-p 'cpp))
  (let ((major-mode-remap-alist '((c++-mode . c++-ts-mode)))
        (prog-mode-hook
         (cons #'reomacs-treesit-rainbow-enable prog-mode-hook))
        (org-src-fontify-natively t))
    (with-temp-buffer
      (insert "#+begin_src C++\nVec<Vec<T>> x;\n#+end_src\n")
      (org-mode)
      (font-lock-ensure)
      (pcase-let ((`(,outer ,inner)
                   (reomacs-treesit-rainbow-test--positions ?<)))
        (should (reomacs-treesit-rainbow-test--face-p
                 outer 'rainbow-delimiters-depth-1-face))
        (should (reomacs-treesit-rainbow-test--face-p
                 inner 'rainbow-delimiters-depth-2-face))))))

;;; reomacs-treesit-rainbow-test.el ends here
