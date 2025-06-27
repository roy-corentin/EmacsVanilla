;;; cr-global-keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 21, 2024

;;; Commentary:


;;; Code:

(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(
     ((or . ((derived-mode . help-mode)
             "\\*\\(eldoc.*\\|vterm-popup-.*\\|helpful.*\\)\\*"))
      ;; List display function
      (display-buffer-in-side-window)
      ;; Parameter
      (side . bottom)
      (dedicated . t)
      (body-function . select-window)
      (slot . 0)
      (window-parameters . ((mode-line-format . none))))
     ("\\*Embark Actions\\*"
      ;; List display function
      (display-buffer-in-side-window)
      ;; Parameter
      (side . bottom)
      (dedicated t)
      (window-height . fit-window-to-buffer))
     ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
      nil
      (window-parameters (mode-line-format . none))))))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'cr-buffer)
;;; cr-buffer.el ends here
