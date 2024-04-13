;;; cr-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 07, 2024
;; Modified: avril 07, 2024
;;;

;; Use evil mode for vim-like keybindings
(use-package evil
  :ensure t
  :general
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-Y-yank-to-eol t
        evil-want-C-u-scroll t
        evil-undo-system 'undo-fu
        evil-move-beyond-eol t
        evil-move-cursor-back nil
        evil-kill-on-visual-paste nil
        evil-symbol-word-search t)
  :bind (:map evil-normal-state-map
              ("TAB" . evil-jump-item))
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (defvar evil-collection-magit-use-z-for-folds t)
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :ensure t
  :init
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  (evil-escape-mode 1))

(use-package evil-org
  :ensure t
  :after evil org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'normal 'evil-org-mode
    (kbd "C-<return>") 'my-org-insert-heading-respect-content-and-prepend-todo
    (kbd "<return>") 'org-open-at-point))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-multiedit
  :ensure t
  :config
  (general-define-key
   :states 'normal
   "M-d" 'evil-multiedit-match-symbol-and-next
   "M-D" 'evil-multiedit-match-symbol-and-prev)
  (general-define-key
   :states 'visual
   "R" 'evil-multiedit-match-all
   "M-d" 'evil-multiedit-match-and-next
   "M-D" 'evil-multiedit-match-and-prev)
  (general-define-key
   :states '(visual normal)
   "C-M-d" 'evil-multiedit-restore)
  (general-define-key
   :states '(normal insert)
   :kemaps '(evil-multiedit-state-map)
   "M-d" 'evil-multiedit-match-and-next
   "M-S-d" 'evil-multiedit-match-and-prev
   "RET" 'evil-multiedit-toggle-or-restrict-region
   "C-n" 'evil-multiedit-next
   "C-p" 'evil-multiedit-prev))

(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode 1))

(provide 'cr-evil)
;;; cr-evil.el ends here
