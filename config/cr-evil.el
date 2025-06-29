;;; cr-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 07, 2024

;;; Commentary:


;;; Code:

(require 'doom-methods)

(use-package evil
  :ensure t
  :custom
  (evil-want-integration t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-C-u-scroll t)
  (evil-undo-system 'undo-fu)
  (evil-move-beyond-eol t)
  (evil-move-cursor-back nil)
  (evil-kill-on-visual-paste nil)
  (evil-symbol-word-search t)
  (evil-ex-search-vim-style-regexp nil)
  (evil-regexp-search t)
  (evil-want-C-i-jump t)
  :config
  (evil-mode 1)
  :init
  (setq evil-want-keybinding nil))

(use-package better-jumper
  :ensure t
  :bind (:map evil-motion-state-map
              ("C-i" . 'better-jumper-jump-forward)
              ("C-o" . 'better-jumper-jump-backward))
  :init
  (better-jumper-mode t))

(use-package evil-indent-plus
  :ensure t
  :after evil
  :init
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down))

(use-package evil-collection
  :ensure t
  :after evil
  :custom
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-z-for-folds t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :ensure t
  :after evil
  :custom
  (evil-escape-excluded-states '(normal visual multiedit emacs motion))
  (evil-escape-excluded-major-modes '(magit-mode treemacs-mode))
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.15)
  (evil-escape-case-insensitive-key-sequence 'case-insensitive)
  :init
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  (evil-escape-mode 1))

(use-package evil-lion
  :ensure t
  :after evil
  :config
  (evil-lion-mode))

(use-package evil-org
  :ensure t
  :hook ((org-mode . evil-org-mode)
         (org-capture-mode . evil-insert-state))
  :custom
  (evil-org-retain-visual-state-on-shift t)
  (evil-org-special-o/O '(table-row))
  (evil-org-use-additional-insert t)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'normal 'evil-org-mode
    (kbd "C-<return>") #'+org/insert-item-below
    (kbd "C-S-<return>") #'+org/insert-item-above
    (kbd "C-M-<return>")  #'org-insert-subheading
    (kbd "S-<return>") #'+org/shift-return
    (kbd "<return>") #'+org/dwim-at-point
    (kbd "zi") #'org-toggle-inline-images
    (kbd "C-S-l") #'org-shiftright
    (kbd "C-S-h") #'org-shiftleft
    (kbd "C-S-k") #'org-shiftup
    (kbd "C-S-j") #'org-shiftdown
    (kbd "]h") #'org-forward-heading-same-level
    (kbd "[h") #'org-backward-heading-same-level
    (kbd "]l") #'org-next-link
    (kbd "[l") #'org-previous-link
    (kbd "]c") #'org-babel-next-src-block
    (kbd "[c") #'org-babel-previous-src-block
    )
  (evil-define-key 'insert 'evil-org-mode
    (kbd "S-<return>") #'+org/shift-return
    (kbd "C-<return>") #'+org/insert-item-below))

(use-package evil-goggles
  :ensure t
  :after evil
  :custom
  (evil-goggles-duration 0.100)
  :config
  (evil-goggles-mode)
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-multiedit
  :ensure t)

(use-package evil-mc
  :ensure t
  :after evil
  :config
  (global-evil-mc-mode 1))

(use-package anzu
  :ensure t
  :after isearch)

(use-package evil-anzu
  :after evil
  :ensure t
  :config
  (global-anzu-mode +1))

(use-package evil-textobj-tree-sitter
  :ensure t
  :after evil
  :init
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  ;; You can also bind multiple items and we will match the first one we can find
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))

(use-package evil-terminal-cursor-changer
  :ensure t
  :hook (tty-setup . evil-terminal-cursor-changer-activate))

(provide 'cr-evil)
;;; cr-evil.el ends here
