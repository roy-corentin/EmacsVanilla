;;; cr-undo.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 08, 2024

;;; Commentary:


;;; Code:

(use-package undo-fu
  :ensure t
  :custom
  (undo-limit 67108864) ; 64mb.
  (undo-strong-limit 100663296) ; 96mb.
  (undo-outer-limit 1006632960) ; 960mb.
  (undo-fu-allow-undo-in-region t)
  (undo-fu-ignore-keyboard-quit t))

(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :config
  (global-undo-fu-session-mode))

(use-package vundo
  :ensure t
  :config
  (vundo-mode))

(provide 'cr-undo)
;;; cr-undo.el ends here
