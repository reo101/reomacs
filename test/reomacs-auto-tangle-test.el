;;; reomacs-auto-tangle-test.el --- Auto-tangle regression -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defconst reomacs-test-config-path
  (expand-file-name "../config.org" (file-name-directory load-file-name)))

;; Load the function without loading the package configuration.
(with-temp-buffer
  (insert-file-contents reomacs-test-config-path)
  (re-search-forward "^[ \t]*(defun reomacs/tangle-config-org\\_>")
  (goto-char (match-beginning 0))
  (eval (read (current-buffer)) t))

(ert-deftest reomacs-auto-tangle-includes-full-buffer-and-restores-narrowing ()
  (with-temp-buffer
    (insert "Bootstrap configuration\nTRAMP configuration\n")
    (let ((buffer-file-name (expand-file-name "config.org" user-emacs-directory))
          (expected (buffer-string))
          observed)
      (goto-char (point-min))
      (forward-line 1)
      (narrow-to-region (point) (point-max))
      (let ((minimum (point-min))
            (maximum (point-max)))
        (cl-letf (((symbol-function 'org-babel-tangle)
                   (lambda () (setq observed (buffer-string)))))
          (reomacs/tangle-config-org))
        (should (equal observed expected))
        (should (buffer-narrowed-p))
        (should (= (point-min) minimum))
        (should (= (point-max) maximum))))))
