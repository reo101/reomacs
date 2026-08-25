;;; reomacs-treesit-rainbow.el --- Tree-sitter rainbow delimiters -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (rainbow-delimiters "2.1.5"))
;; Keywords: faces, languages, tree-sitter

;;; Commentary:

;; Add depth-based delimiter faces to native tree-sitter font lock.
;; `rainbow-delimiters-mode' remains the fallback for modes without a
;; supported tree-sitter parser.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'treesit)
(require 'rainbow-delimiters)

(defconst reomacs-treesit-rainbow--common-pairs
  '(("(" . ")") ("[" . "]") ("{" . "}")))

(defconst reomacs-treesit-rainbow--language-specs
  '((cpp
     :tokens ("(" ")" "[" "]" "{" "}" "<" ">")
     :semantic (("template_argument_list" . (("<" . ">")))
                ("template_parameter_list" . (("<" . ">")))))
    (rust
     :tokens ("(" ")" "[" "]" "{" "}" "<" ">" "|")
     :semantic (("type_arguments" . (("<" . ">")))
                ("type_parameters" . (("<" . ">")))
                ("closure_parameters" . (("|" . "|")))))
    (typescript
     :tokens ("(" ")" "[" "]" "{" "}" "<" ">")
     :semantic (("type_arguments" . (("<" . ">")))
                ("type_parameters" . (("<" . ">")))))
    (tsx
     :tokens ("(" ")" "[" "]" "{" "}" "<" ">")
     :semantic (("type_arguments" . (("<" . ">")))
                ("type_parameters" . (("<" . ">"))))))
  "Languages where syntax tables cannot identify every delimiter.")

(defvar-local reomacs-treesit-rainbow--language nil)
(defvar-local reomacs-treesit-rainbow--installed-p nil)
(defvar-local reomacs-treesit-rainbow--original-settings nil)
(defvar-local reomacs-treesit-rainbow--original-features nil)

(defun reomacs-treesit-rainbow--spec (language)
  "Return the delimiter specification for LANGUAGE."
  (cdr (assq language reomacs-treesit-rainbow--language-specs)))

(defun reomacs-treesit-rainbow--parser ()
  "Return the first parser with a supported language in this buffer."
  (seq-find (lambda (parser)
              (reomacs-treesit-rainbow--spec
               (treesit-parser-language parser)))
            (treesit-parser-list)))

(defun reomacs-treesit-rainbow-supported-p ()
  "Return non-nil when the current buffer has a supported parser."
  (and (treesit-available-p)
       treesit-font-lock-settings
       (reomacs-treesit-rainbow--parser)))

(defun reomacs-treesit-rainbow--semantic-pairs (language node-type)
  "Return semantic delimiter pairs for NODE-TYPE in LANGUAGE."
  (cdr (assoc node-type
              (plist-get (reomacs-treesit-rainbow--spec language)
                         :semantic))))

(defun reomacs-treesit-rainbow--delimiter-eligible-p (node language)
  "Return non-nil when delimiter NODE is meaningful in LANGUAGE."
  (let ((token (treesit-node-type node)))
    (or (assoc token reomacs-treesit-rainbow--common-pairs)
        (rassoc token reomacs-treesit-rainbow--common-pairs)
        (when-let ((parent (treesit-node-parent node)))
          (seq-some (lambda (pair)
                      (or (string= token (car pair))
                          (string= token (cdr pair))))
                    (reomacs-treesit-rainbow--semantic-pairs
                     language (treesit-node-type parent)))))))

(defun reomacs-treesit-rainbow--pair-encloses-p (node position pair)
  "Return non-nil when a direct delimiter PAIR of NODE encloses POSITION."
  (let ((open (car pair))
        (close (cdr pair))
        open-position
        close-position
        (child (treesit-node-child node 0)))
    (while child
      (let ((type (treesit-node-type child)))
        (if (string= open close)
            (when (string= type open)
              (if open-position
                  (setq close-position (treesit-node-end child))
                (setq open-position (treesit-node-start child))))
          (cond
           ((and (null open-position) (string= type open))
            (setq open-position (treesit-node-start child)))
           ((string= type close)
            (setq close-position (treesit-node-end child))))))
      (setq child (treesit-node-next-sibling child)))
    (and open-position close-position
         (<= open-position position)
         (<= position close-position))))

(defun reomacs-treesit-rainbow--node-encloses-p (node position language)
  "Return non-nil when a delimiter pair in NODE encloses POSITION.
LANGUAGE selects any grammar-specific semantic pairs."
  (seq-some (lambda (pair)
              (reomacs-treesit-rainbow--pair-encloses-p
               node position pair))
            (append reomacs-treesit-rainbow--common-pairs
                    (reomacs-treesit-rainbow--semantic-pairs
                     language (treesit-node-type node)))))

(defun reomacs-treesit-rainbow--depth (node language)
  "Return the delimiter nesting depth for NODE in LANGUAGE."
  (let ((position (treesit-node-start node))
        (parent (treesit-node-parent node))
        (depth 0))
    (while parent
      (when (reomacs-treesit-rainbow--node-encloses-p
             parent position language)
        (cl-incf depth))
      (setq parent (treesit-node-parent parent)))
    (max 1 depth)))

(defun reomacs-treesit-rainbow--face (depth position)
  "Return the configured rainbow face for DEPTH at POSITION."
  (funcall rainbow-delimiters-pick-face-function depth t position))

(defun reomacs-treesit-rainbow--fontify-elisp (end)
  "Fontify Elisp string delimiters and keywords through END."
  (while (re-search-forward
          "\\(\"\\|\\_<:\\(?:\\sw\\|\\s_\\)+\\_>\\)" end t)
    (let* ((start (match-beginning 0))
           (finish (match-end 0))
           (state (syntax-ppss start))
           face)
      (cond
       ((eq (char-after start) ?\")
        (unless (eq (nth 3 state) (nth 3 (syntax-ppss (1+ start))))
          (setq face
                (reomacs-treesit-rainbow--face
                 (1+ (nth 0 state)) start))))
       ((and (> (nth 0 state) 0)
             (not (nth 3 state))
             (not (nth 4 state)))
        (setq face
              (reomacs-treesit-rainbow--face
               (1+ (nth 0 state)) start))))
      (when face
        (font-lock-prepend-text-property start finish 'face face))
      (goto-char finish)))
  nil)

(defconst reomacs-treesit-rainbow--elisp-keywords
  '(reomacs-treesit-rainbow--fontify-elisp))

(define-minor-mode reomacs-treesit-rainbow-elisp-mode
  "Color Elisp string delimiters and keywords by nesting depth."
  :lighter ""
  (font-lock-remove-keywords
   nil reomacs-treesit-rainbow--elisp-keywords)
  (when reomacs-treesit-rainbow-elisp-mode
    (font-lock-add-keywords
     nil reomacs-treesit-rainbow--elisp-keywords 'append))
  (when font-lock-mode (font-lock-flush)))

(defun reomacs-treesit-rainbow--fontify-delimiter
    (node override start end &rest _)
  "Fontify delimiter NODE between START and END using OVERRIDE."
  (when (reomacs-treesit-rainbow--delimiter-eligible-p
         node reomacs-treesit-rainbow--language)
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     (reomacs-treesit-rainbow--face
      (reomacs-treesit-rainbow--depth
       node reomacs-treesit-rainbow--language)
      (treesit-node-start node))
     override start end)))

(defun reomacs-treesit-rainbow--settings (language spec)
  "Build native font-lock settings for LANGUAGE using SPEC."
  (treesit-font-lock-rules
   :language language
   :feature 'rainbow-delimiters
   :override 'prepend
   (list (vconcat (plist-get spec :tokens))
         '@reomacs-treesit-rainbow--fontify-delimiter)))

(defun reomacs-treesit-rainbow--teardown ()
  "Remove tree-sitter rainbow font-lock settings from this buffer."
  (when reomacs-treesit-rainbow--installed-p
    (setq-local treesit-font-lock-settings
                reomacs-treesit-rainbow--original-settings)
    (setq-local treesit-font-lock-feature-list
                reomacs-treesit-rainbow--original-features)
    (treesit-font-lock-recompute-features)
    (when font-lock-mode (font-lock-flush)))
  (setq reomacs-treesit-rainbow--language nil
        reomacs-treesit-rainbow--installed-p nil
        reomacs-treesit-rainbow--original-settings nil
        reomacs-treesit-rainbow--original-features nil))

(defun reomacs-treesit-rainbow--setup ()
  "Install tree-sitter rainbow font-lock settings in this buffer."
  (reomacs-treesit-rainbow--teardown)
  (when-let* ((parser (reomacs-treesit-rainbow--parser))
              (language (treesit-parser-language parser))
              (spec (reomacs-treesit-rainbow--spec language)))
    (setq reomacs-treesit-rainbow--language language
          reomacs-treesit-rainbow--installed-p t
          reomacs-treesit-rainbow--original-settings
          treesit-font-lock-settings
          reomacs-treesit-rainbow--original-features
          treesit-font-lock-feature-list)
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings
                        (reomacs-treesit-rainbow--settings language spec)))
    (setq-local treesit-font-lock-feature-list
                (cons (cons 'rainbow-delimiters
                            (car treesit-font-lock-feature-list))
                      (cdr treesit-font-lock-feature-list)))
    (treesit-font-lock-recompute-features)
    (when font-lock-mode (font-lock-flush))
    t))

;;;###autoload
(define-minor-mode reomacs-treesit-rainbow-mode
  "Color delimiters by depth using native tree-sitter font lock."
  :lighter ""
  (if reomacs-treesit-rainbow-mode
      (condition-case err
          (unless (reomacs-treesit-rainbow--setup)
            (setq reomacs-treesit-rainbow-mode nil))
        (treesit-query-error
         (reomacs-treesit-rainbow--teardown)
         (setq reomacs-treesit-rainbow-mode nil)
         (message "Tree-sitter rainbow disabled: %s"
                  (error-message-string err))))
    (reomacs-treesit-rainbow--teardown)))

;;;###autoload
(defun reomacs-treesit-rainbow-enable ()
  "Enable semantic tree-sitter rainbows, or syntax-table fallback."
  (rainbow-delimiters-mode -1)
  (reomacs-treesit-rainbow-elisp-mode -1)
  (if (reomacs-treesit-rainbow-supported-p)
      (progn
        (reomacs-treesit-rainbow-mode 1)
        (unless reomacs-treesit-rainbow-mode
          (rainbow-delimiters-mode 1)))
    (reomacs-treesit-rainbow-mode -1)
    (rainbow-delimiters-mode 1)
    (when (derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode)
      (reomacs-treesit-rainbow-elisp-mode 1))))

(defun reomacs-treesit-rainbow-refresh ()
  "Refresh rainbow delimiter modes in existing programming buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'prog-mode)
        (reomacs-treesit-rainbow-enable)))))

(provide 'reomacs-treesit-rainbow)
;;; reomacs-treesit-rainbow.el ends here
