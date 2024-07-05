;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

;; Load the package manager and initialize MELPA

(use-package which-key
  :ensure nil
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
  :hook (prog-mode text-mode)
  :custom
  (sp-highlight-pair-overlay nil)
  :init
  (require 'smartparens-config))

(use-package ace-window
  :ensure t
  :defer t
  :init
  (global-set-key [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'global
        aw-background t))

(use-package olivetti
  :ensure t
  :defer t
  :bind ("C-c o" . olivetti-mode)
  :custom
  (olivetti-body-width 130)
  :hook (text-mode magit-mode))

(use-package casual-calc
  :ensure t
  :defer t
  :bind (:map calc-mode-map ("C-o" . 'casual-main-menu)))

(use-package vterm
  :ensure t
  :defer t
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000))

(use-package pdf-tools
  :after evil
  :ensure t)

(use-package copilot
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "copilot-emacs/copilot.el" :files (:defaults))
  :defer t
  :bind (:map copilot-completion-map
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion)))

(use-package keycast
  :ensure t
  :defer t)

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

(use-package transient
  :ensure t)

(use-package posframe
  :ensure t)

(use-package compile
  :ensure nil
  :config
  (push 'bun-test compilation-error-regexp-alist)
  (push '(bun-test "\sat\s\\([a-zA-Z0-9/\\._-]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)
        compilation-error-regexp-alist-alist))

(require 'cr-buffer)
(require 'cr-magit)
(require 'cr-theme)
(require 'cr-yasnippet)
(require 'cr-eglot)
(require 'cr-project)
(require 'cr-dashboard)
(require 'cr-completion)
(require 'cr-undo)
(require 'cr-treesit)
(require 'cr-org)
(require 'cr-treemacs)
(require 'cr-meow)
(require 'cr-evil)
(require 'cr-dired)
(require 'cr-language)
(require 'cr-debugger)

;; Key set at the end to avoid conflicts with iedit
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-;") 'next-buffer)
