;;; -*- lexical-binding: t -*-

(let ((config-org (expand-file-name "config.org" user-emacs-directory))
      (config-el (expand-file-name "config.el" user-emacs-directory)))
  (when (and (file-exists-p config-org)
             (not (file-exists-p config-el)))
    (require 'org) ;; ensure org is loaded before tangling
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle-file config-org)))

  ;; Load the resulting config.el (tangled or preexisting)
  (when (file-exists-p config-el)
    (load-file config-el)))
