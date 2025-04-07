;;; cr-formatter.el --- Config for formatter packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Corentin ROY

;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: février 12, 2025

(use-package apheleia
  :ensure t
  :config
  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))
  (apheleia-global-mode +1))


(use-package stripspace
  :ensure t
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))
  :custom
  (stripspace-only-if-initially-clean nil)
  (stripspace-restore-column t))

(provide 'cr-formatter)
;;; cr-formatter.el ends here
