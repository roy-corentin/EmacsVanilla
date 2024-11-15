;;; cr-olivetti.el --- Custom olivetti-mode setup    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: Novembre 14, 2024

(defun cr/set-flymake-indicator-type ()
  (setq-local flymake-indicator-type (if olivetti-mode 'fringes 'margins)))

(use-package flymake
  :ensure nil
  :custom
  (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-indicator-type 'margins)
  :init
  (add-hook 'olivetti-mode-hook #'cr/set-flymake-indicator-type))

(provide 'cr-flymake)
