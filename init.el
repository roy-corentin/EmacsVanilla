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
  :hook (prog-mode text-mode)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'minibuffer-mode "'" nil :actions nil)
  (sp-local-pair 'elisp-mode "'" nil :actions nil))

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
  :after evil
  :ensure t)

(use-package copilot
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "copilot-emacs/copilot.el" :files (:defaults))
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

(use-package transient
  :ensure t)

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

;; Key set at the end to avoid conflicts with iedit
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-;") 'next-buffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" default))
 '(package-selected-packages
   '(svg-tag-mode yasnippet-capf ruby-end load-env-vars crystal-mode solaire-mode verb verb-mode zig-mode emacs-surround which-key wgrep vundo vterm vi-tilde-fringe vertico undo-fu-session undo-fu treesit-auto treemacs-nerd-icons treemacs-magit treemacs-evil toc-org snippets smartparens restclient rainbow-delimiters pdf-tools org-roam-ui org-bullets orderless olivetti nerd-icons-dired nerd-icons-corfu nerd-icons-completion mixed-pitch meow markdown-mode marginalia magit-todos keycast git-gutter general evil-surround evil-org evil-multiedit evil-mc evil-lion evil-goggles evil-escape evil-collection evil-anzu embark-consult elixir-ts-mode eglot-booster eglot editorconfig drag-stuff doom-themes doom-modeline docker diredfl dashboard csv-mode csv corfu copilot casual cape auto-yasnippet apheleia)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
