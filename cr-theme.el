;;; cr-keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 09, 2024

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-moonlight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package hide-line-mode :ensure t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :init
  (setq projectile-dynamic-mode-line nil)
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-mode 1)

  :config
  (add-hook 'doom-load-theme-hook #'doom-modeline-refresh-bars)
  ;; (add-hook 'magit-mode-hook
  ;;           (defun +modeline-hide-in-non-status-buffer-h ()
  ;;             "Show minimal modeline in magit-status buffer, no modeline elsewhere."
  ;;             (if (eq major-mode 'magit-status-mode)
  ;;                 (doom-modeline-set-modeline 'magit)
  ;;               (hide-mode-line-mode))))

  ;;; Extensions
  (use-package anzu
    :ensure t
    :after isearch)

  (use-package evil-anzu
    :after evil
    :ensure t
    :config (global-anzu-mode +1)))

(provide 'cr-theme)
