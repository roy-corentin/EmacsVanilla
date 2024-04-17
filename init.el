;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

;; Load the package manager and initialize MELPA
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Initialize load path for loading configuration files
(add-to-list 'load-path "~/.config/emacs_vanilla/config/")

;; Ensure that use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load use-package
(eval-when-compile
  (require 'use-package))

(use-package emacs
  :ensure nil
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (delete-selection-mode 1)
  (global-hl-line-mode 1)
  (tab-bar-mode 1)
  (desktop-save-mode 1)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-ts-mode 'display-line-numbers-mode)
  (add-hook 'org-mode-hook 'display-line-numbers-mode)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  :custom
  (tab-bar-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-new-tab-choice "*dashboard*")
  (tab-bar-auto-width nil)
  (tab-bar-separator (propertize "|"))
  (tab-bar-format '(tab-bar-format-tabs (lambda () " ")))
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)
  (fill-column 80)
  (inhibit-startup-screen t)
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  ;; (show-trailing-whitespace t)
  (column-number-mode t)
  (display-line-numbers-type 'visual)
  (display-line-numbers-width 3)
  (display-line-numbers-current-absolute t)
  (indent-tabs-mode nil)
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :custom-face
  (default ((t :family "JetBrains Mono Nerd Font" :height 105)))
  (fixed-pitch ((t :family "JetBrains Mono Nerd Font" :height 105)))
  (variable-pitch ((t :family "C059" :height 115)))
  :bind
  ("C-=" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("C-+" . (lambda () (interactive) (text-scale-set 0)))
  )

(use-package gcmh
  :defer t
  :config
  (gcmh-mode 1))

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
  :bind (:map calc-mode-map ("C-o" . 'casual-main-menu)))

(use-package restclient
  :ensure t)

(use-package vterm
  :ensure t
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000))

(use-package pdf-tools
  :ensure t)

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
(require 'cr-evil)
(require 'cr-dired)
(require 'cr-global-keybindings)

;; Key set at the end to avoid conflicts with iedit
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-;") 'next-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auto-yasnippet snippets magit-todos eglot-booster docker markdown-mode wgrep org-roam-ui websocket csv-mode csv evil-goggles evil-mc evil-multiedit evil-org which-key vundo vi-tilde-fringe vertico undo-fu-session undo-fu treesit-auto treemacs-nerd-icons treemacs-magit treemacs-evil toc-org smartparens rainbow-delimiters org-roam org-bullets orderless olivetti nerd-icons-dired nerd-icons-corfu nerd-icons-completion mixed-pitch marginalia hl-todo git-gutter general evil-surround evil-escape evil-collection evil-anzu embark-consult elixir-ts-mode drag-stuff doom-themes doom-modeline diredfl dashboard corfu cape apheleia))
 '(package-vc-selected-packages
   '((snippets :vc-backend Git :url "https://github.com/doomemacs/snippets")
     (eglot-booster :vc-backend Git :url "https://github.com/jdtsmith/eglot-booster"))))
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
