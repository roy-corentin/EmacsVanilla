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
  :custom
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-no-changes-timeout 3))

(use-package flyover
  :ensure t
  :disabled (eql nil flymake-show-diagnostics-at-end-of-line)
  :hook flymake-mode
  :custom
  (flyover-levels '(error warning info))
  (flyover-checkers '(flymake))
  (flyover-use-theme-colors t)
  (flyover-wrap-messages t))

(provide 'cr-flymake)
;;; cr-flymake.el ends here
