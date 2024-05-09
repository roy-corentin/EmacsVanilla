;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

;; Load the package manager and initialize MELPA

(use-package which-key
  :ensure t
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-popup-type 'side-window)
  (push '(("" ."\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil .  "◂\\1")) which-key-replacement-alist)
  (which-key-mode))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package smartparens-mode
  :ensure smartparens
  :hook (prog-mode text-mode))

(use-package ace-window
  :ensure t
  :defer t
  :init
  (global-set-key [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame
        aw-background t))

(use-package olivetti
  :ensure t
  :defer t
  :bind ("C-c o" . olivetti-mode)
  :custom
  (olivetti-body-width 120)
  :hook (text-mode magit-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode))

(use-package csv
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package docker
  :ensure t)

(use-package casual
  :ensure t
  :defer t
  :bind (:map calc-mode-map ("C-o" . 'casual-main-menu)))

(use-package restclient
  :ensure t)

(use-package vterm
  :ensure t
  :defer t
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000))

(use-package pdf-tools
  :ensure t)

(use-package copilot
  :defer t
  :bind (:map copilot-completion-map
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion)))

(use-package keycast
  :ensure t)

(use-package verb
  :ensure t)

(use-package solaire-mode
  :ensure t
  :init
  (solaire-global-mode t))

(use-package load-env-vars
  :ensure t
  :config
  (load-env-vars (concat user-emacs-directory ".env")))

(require 'cr-language)
(require 'cr-buffer)
(require 'cr-magit)
(require 'cr-yasnippet)
(require 'cr-eglot)
(require 'cr-project)
(require 'cr-theme)
(require 'cr-dashboard)
(require 'cr-completion)
(require 'cr-undo)
(require 'cr-treesit)
(require 'cr-org)
(require 'cr-treemacs)
(require 'cr-meow)
(require 'cr-evil)
(require 'cr-dired)

;; Key set at the end to avoid conflicts with iedit
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-;") 'next-buffer)
