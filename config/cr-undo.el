;;; cr-undo.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 08, 2024

(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t
  :config
  (global-undo-fu-session-mode))

(use-package vundo
  :ensure t
  :demand t
  :config
  (vundo-mode))

(provide 'cr-undo)
;;; cr-undo.el ends here
