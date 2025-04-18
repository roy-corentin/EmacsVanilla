;;; cr-olivetti.el --- Custom olivetti-mode setup    -*- lexical-binding: t; -*-
;;; Commentary:

;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: Novembre 14, 2024

;;; Code:

(use-package flymake
  :ensure nil
  :after olivetti
  :preface
  (defun cr/set-flymake-indicator-type ()
    (setq-local flymake-indicator-type (if olivetti-mode 'fringes 'margins)))
  :hook (prog-mode . cr/set-flymake-indicator-type)
  :custom
  (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-indicator-type 'margins))

(provide 'cr-flymake)
;;; cr-flymake.el ends here
