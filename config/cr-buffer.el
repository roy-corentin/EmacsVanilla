;;; cr-global-keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 21, 2024

(setq display-buffer-alist
      '(
        ((or . ((derived-mode . help-mode)
                "\\*\\(eldoc\\|vterm-popup-.*\\)\\*"))
         ;; List display function
         (display-buffer-in-side-window)
         ;; Parameter
         (side . bottom)
         (dedicated . t)
         (body-function . (lambda (window) (select-window window)))
         (slot . 0)
         (window-parameters . ((mode-line-format . none))))
        ))

(provide 'cr-buffer)
