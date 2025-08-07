;;; cr-flymake.el --- Flymake setup    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: Novembre 14, 2024

;;; Commentary:

;;; Code:

(use-package flymake
  :preface
  (defun flymake-setup-show-diagnostis-style ()
    (setq flymake-show-diagnostics-at-end-of-line (if (display-graphic-p) nil 'fancy)))
  :hook prog-mode
  ;; :hook (before-make-frame . flymake-setup-show-diagnostis-style)
  :custom
  (flymake-show-diagnostic-at-end-of-line 'fancy)
  (flymake-no-changes-timeout 0.25)
  (flymake-indicator-type 'margins))

(use-package flyover
  :ensure (:host github :repo "konrad1977/flyover" :depth 1)
  :disabled
  :hook flymake-mode
  :custom
  (flyover-levels '(error warning info))
  (flyover-checkers '(flymake))
  (flyover-use-theme-colors t)
  (flyover-wrap-messages t))

(provide 'cr-flymake)
;;; cr-flymake.el ends here
