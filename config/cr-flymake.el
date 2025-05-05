;;; cr-flymake.el --- Flymake setup    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: Novembre 14, 2024

;;; Commentary:

;;; Code:

(use-package flymake
  :hook (olivetti-mode-on . (lambda () (setq-local flymake-indicator-type 'fringes)))
  :hook (olivetti-mode-off . (lambda () (setq-local flymake-indicator-type 'margins)))
  :custom
  (flymake-no-changes-timeout 0.25)
  (flymake-show-diagnostics-at-end-of-line 'fancy)
  (flymake-indicator-type 'margins))

(provide 'cr-flymake)
;;; cr-flymake.el ends here
