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

(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

(provide 'cr-formatter)
;;; cr-formatter.el ends here
