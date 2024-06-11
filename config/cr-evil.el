;;; cr-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 07, 2024

(require 'doom-methods)

;; Use evil mode for vim-like keybindings
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t
        evil-want-C-u-scroll t
        evil-undo-system 'undo-fu
        evil-move-beyond-eol t
        evil-move-cursor-back nil
        evil-kill-on-visual-paste nil
        evil-symbol-word-search t)
  :bind
  (:map evil-normal-state-map
        ("TAB" . evil-jump-item))
  (:map evil-visual-state-map
        ("TAB" . evil-jump-item))
  (:map evil-normal-state-map
        ("C-i" . evil-jump-forward))
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-collection-magit
  :defer t
  :init (defvar evil-collection-magit-use-z-for-folds t)
  :config
  ;; Some extra vim-isms I thought were missing from upstream
  (evil-define-key* '(normal visual) magit-mode-map
    "*"  #'magit-worktree
    "zt" #'evil-scroll-line-to-top
    "zz" #'evil-scroll-line-to-center
    "zb" #'evil-scroll-line-to-bottom
    "g=" #'magit-diff-default-context
    "gi" #'forge-jump-to-issues
    "gm" #'forge-jump-to-pullreqs)
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :ensure t
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(magit-mode treemacs-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  (evil-escape-mode 1))

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-org
  :ensure t
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state)
  :init
  (defvar evil-org-retain-visual-state-on-shift t)
  (defvar evil-org-special-o/O '(table-row))
  (defvar evil-org-use-additional-insert t)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'normal 'evil-org-mode
    (kbd "C-<return>") '+org/insert-item-below
    (kbd "<return>") '+org/dwim-at-point
    (kbd "zi") 'org-toggle-inline-images)
  (evil-define-key 'insert 'evil-org-mode
    (kbd "C-<return>") '+org/insert-item-below))

(use-package evil-goggles
  :ensure t
  :custom
  (evil-goggles-duration 0.100)
  :config
  (evil-goggles-mode)
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-multiedit :ensure t)

(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode 1))

(use-package anzu
  :ensure t
  :after isearch)
(use-package evil-anzu
  :after evil
  :ensure t
  :config (global-anzu-mode +1))

(require 'cr-global-keybindings)

(provide 'cr-evil)
;;; cr-evil.el ends here
