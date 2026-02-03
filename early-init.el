;;; -*- lexical-binding: t -*-

;; Disable package.el entirely in favor of straight.el
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

;; Native compilation (Emacs 28+)
(when (featurep 'native-compile)
  (setq native-comp-jit-compilation t
        native-comp-async-report-warnings-errors 'silent))

;; Prefer newer source files over stale bytecode
(setq load-prefer-newer t)
