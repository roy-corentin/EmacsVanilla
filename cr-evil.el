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
  :init
  (setq evil-want-Y-yank-to-eol t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-undo-system 'undo-fu
        evil-move-beyond-eol t
        evil-move-cursor-back nil
        evil-kill-on-visual-paste nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
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

(use-package evil-collection-magit
  :defer t
  :init (defvar evil-collection-magit-use-z-for-folds t)
  :config
  (evil-defined-key* 'normal magit-status-mode-map [escape] nil))

(provide 'cr-evil)
;;; cr-evil.el ends here
