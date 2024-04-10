;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

(require 'eglot)
(require 'cr-project)
(require 'cr-theme)
(require 'cr-dashboard)
(require 'cr-evil)
(require 'cr-keybindings)
(require 'cr-completion)
(require 'cr-undo)
(require 'cr-treesit)
(require 'cr-org)
(require 'cr-treemacs)

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

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-find-file)) ; use dired-open-file or dired-find-file instead if not using dired-open package

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

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

(provide 'init)
