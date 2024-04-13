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
  (add-hook 'org-mode-hook 'display-line-numbers-mode)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  :custom
  (tab-bar-show nil)
  (tab-bar-new-tab-choice "*dashboard*")
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)
  (fill-column 80)
  (inhibit-startup-screen t)
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  ;; (show-trailing-whitespace t)
  ;; (column-number-mode t)
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
  (which-key-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil))

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

(require 'eglot)
(require 'cr-project)
(require 'cr-theme)
(require 'cr-dashboard)
(require 'cr-keybindings)
(require 'cr-completion)
(require 'cr-undo)
(require 'cr-treesit)
(require 'cr-eglot)
(require 'cr-org)
(require 'cr-treemacs)
(require 'cr-evil)
(require 'cr-dired)

;; Key set at the end to avoid conflicts with iedit
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-;") 'next-buffer)
