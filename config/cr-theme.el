;;; cr-keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 09, 2024

;;; Commentary:

;;; Code:

(use-package doom-themes
  :ensure t
  :demand t
  :custom
  (doom-themes-enable-bold t) ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode t))

(use-package doom-modeline
  :ensure t
  :hook (doom-modeline-mode . column-number-mode)
  :hook (emacs-startup . doom-modeline-refresh-bars)
  :custom
  (doom-modeline-bar-width 3)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-position-column-line-format '("%l:%c"))
  (doom-modeline-minor-modes nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-workspace-name nil)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-project-name t)
  :config
  (doom-modeline-mode 1))

(use-package catppuccin-theme
  :ensure t
  :demand t
  :custom
  (catppuccin-flavor 'mocha))

(use-package kaolin-themes
  :ensure t
  :demand t
  :custom
  (kaolin-themes-distinct-tab-line t)
  (kaolin-themes-hl-line-colored t))

(use-package lambda-themes
  :ensure (:host github :repo "lambda-emacs/lambda-themes")
  :demand t
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t))

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(use-package doom-two-tone-themes
  :demand t
  :ensure (:host github :repo "eliraz-refael/doom-two-tone-themes" :depth 2 :files (:defaults "themes/*el")))

(defun load-custom-theme ()
  "Load custom theme."
  (interactive)
  (load-theme (or emacs-theme 'modus-vivendi) t))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-custom-theme)
                  (set-frame-parameter frame 'alpha-background default-opacity))))
  (add-hook 'emacs-startup-hook #'load-custom-theme)
  (set-frame-parameter nil 'alpha-background default-opacity))

(provide 'cr-theme)
;;; cr-theme.el ends here
