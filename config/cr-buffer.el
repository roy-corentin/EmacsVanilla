;;; cr-global-keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 21, 2024

;;; Commentary:


;;; Code:

(defun cr-buffer-has-project-p (buffer action)
  "BUFFER is actual buffer.  ACTION is list of action."
  (with-current-buffer buffer (project-current nil)))

(defun cr-tab-tab-name (buffer alist)
  "BUFFER is actual buffer.  ALIST is list of parameters."
  (with-current-buffer buffer (project-name (project-current))))

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
      (window-parameters (mode-line-format . none)))
     (".*todo.org"
      ;; List display function
      (display-buffer-in-tab)
      ;; Parameter
      (tab-name . "todo"))
     ("^\\*elfeed-entry-"
      (display-buffer-in-tab)
      (dedicated . t)
      (tab-name . (lambda (buffer alist)
                    (with-current-buffer buffer
                      (elfeed-feed-title (elfeed-entry-feed elfeed-show-entry)))))
      (tab-group . "Elfeed"))
     ("\\*elfeed-search\\*"
      (display-buffer-in-tab)
      (dedicated . t)
      (tab-name . "Entries")
      (tab-group . "Elfeed"))
     (cr-buffer-has-project-p
      ;; List display function
      (display-buffer-in-tab)
      ;; Parameter
      (tab-name . cr-tab-tab-name)))))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'cr-buffer)
;;; cr-buffer.el ends here
