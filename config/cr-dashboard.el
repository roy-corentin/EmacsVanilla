;;; cr-dashboard.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 08, 2024

;; use-package with package.el:

(use-package dashboard
  :ensure t
  :hook (elpaca-after-init . dashboard-insert-startupify-lists)
  :hook (elpaca-after-init . dashboard-initialize)
  :requires nerd-icons
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-items '((recents   . 5)
                     (projects  . 5)
                     (agenda    . 5)
                     (bookmarks . 5)
                     (registers . 5)))
  :config
  (dashboard-setup-startup-hook))

(provide 'cr-dashboard)
;;; cr-dashboard.el ends here
