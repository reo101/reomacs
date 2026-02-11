;;; -*- lexical-binding: t -*-

;; Check minimum Emacs version (29+ for treesit, 30+ for some features)
(when (< emacs-major-version 29)
  (error "Reomacs requires Emacs 29+, you have %s" emacs-version))

;; Strategy: Always load pre-tangled config.el directly.
;; This avoids loading built-in org before straight can install a custom version.
;; Tangling happens automatically on save (see auto-tangle in config.org).
;; If config.el is missing, we fall back to tangling with built-in org.

(let ((config-org (expand-file-name "config.org" user-emacs-directory))
      (config-el (expand-file-name "config.el" user-emacs-directory)))
  (cond
   ;; Prefer pre-tangled config.el (allows custom org version)
   ((file-exists-p config-el)
    (load-file config-el))
   ;; Fallback: tangle with built-in org (first-time bootstrap only)
   ((file-exists-p config-org)
    (require 'org)
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle-file config-org))
    (load-file config-el))
   (t
    (warn "Neither config.el nor config.org found in %s" user-emacs-directory))))
