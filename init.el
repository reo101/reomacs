;;; -*- lexical-binding: t -*-

;; Check minimum Emacs version (29+ for treesit, 30+ for some features)
(when (< emacs-major-version 29)
  (error "Reomacs requires Emacs 29+, you have %s" emacs-version))

;; Set up the native Android GUI before loading the package-heavy config.
;; The GUI owns the Emacs server; NoD shells connect through its Unix socket.
(when (eq system-type 'android)
  (setq temporary-file-directory
        (file-name-as-directory
         (expand-file-name "cache/nns" user-emacs-directory))
        create-lockfiles nil
        make-backup-files nil)
  (make-directory temporary-file-directory t)
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (require 'nns)
  (nns-mode 1)

  (require 'server)
  (setq server-socket-dir "/data/nix/var/emacs-server")
  ;; The directory belongs to Emacs, with search-only access for NoD.
  ;; Upstream rejects any group access, including search without write.
  (defun reomacs/android-server-ensure-safe-dir (original directory)
    "Accept only the provisioned, search-only Android server DIRECTORY."
    (if (equal (directory-file-name directory) server-socket-dir)
        (let ((attributes (file-attributes directory 'integer)))
          (unless (and (eq t (file-attribute-type attributes))
                       (= (file-attribute-user-id attributes) (user-uid))
                       (= (file-modes directory 'nofollow) #o2710))
            (error "Unsafe Android Emacs server directory: %s" directory)))
      (funcall original directory)))
  (when (file-directory-p server-socket-dir)
    (advice-add 'server-ensure-safe-dir :around
                #'reomacs/android-server-ensure-safe-dir)
    (server-start)
    (set-file-modes (expand-file-name server-name server-socket-dir) #o770)))

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
