;;; cr-dashboard.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 08, 2024

;; use-package with package.el:

(defun my-dashboard-setup-startup-hook ()
  (add-hook 'window-size-change-functions #'dashboard-resize-on-hook 100)
  (add-hook 'window-setup-hook #'dashboard-resize-on-hook)
  (add-hook 'after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'emacs-startup-hook #'dashboard-initialize))

(use-package dashboard
  :ensure t
  :requires nerd-icons
  :init
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (agenda    . 5)
                          (bookmarks . 5)
                          (registers . 5)))
  :config
  (my-dashboard-setup-startup-hook))

(provide 'cr-dashboard)
;;; cr-dashboard.el ends here
