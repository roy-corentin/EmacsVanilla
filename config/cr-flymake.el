;;; cr-flymake.el --- Flymake setup    -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: Novembre 14, 2024

;;; Commentary:

;;; Code:

(use-package flymake
  :ensure nil
  :hook prog-mode
  :hook (tty-setup . (lambda () (setq flymake-show-diagnostics-at-end-of-line 'fancy)))
  :custom
  (flymake-show-diagnostics-at-end-of-line 'fancy)
  (flymake-no-changes-timeout 0.25))

(use-package flyover
  :ensure t
  :disabled t
  :hook flymake-mode
  :custom
  (flyover-levels '(error warning info))
  (flyover-checkers '(flymake))
  (flyover-use-theme-colors t)
  (flyover-wrap-messages t))

(provide 'cr-flymake)
;;; cr-flymake.el ends here
