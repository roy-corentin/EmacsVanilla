;;; cr-flymake.el --- Flymake setup    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: Novembre 14, 2024

;;; Commentary:

;;; Code:

(use-package flymake
  :defer t
  :preface
  (defun set-flymake-fringes-indicator()
    (setq-local flymake-indicator-type 'fringes))
  (defun set-flymake-margins-indicator()
    (setq-local flymake-indicator-type 'margins))
  :hook (olivetti-mode-on . set-flymake-fringes-indicator)
  :hook (olivetti-mode-off . set-flymake-margins-indicator)
  :hook emacs-lisp-mode
  :custom
  (flymake-no-changes-timeout 0.25)
  (flymake-show-diagnostics-at-end-of-line 'fancy)
  (flymake-indicator-type 'margins))

(provide 'cr-flymake)
;;; cr-flymake.el ends here
