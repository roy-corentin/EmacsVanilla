;;; init.el --- Terminal Config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

;;; Commentary:

;;; Code:

(use-package vterm
  :ensure t
  :preface
  (defun disable-trailing-whitespace ()
    (setq-local show-trailing-whitespace nil))
  :hook (vterm-mode. disable-trailing-whitespace)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000)
  :config
  (evil-define-key 'insert vterm-mode-map
    (kbd "C-p") 'cr/vterm-insert-up
    (kbd "C-n") 'cr/vterm-insert-down))

;; Add support for the Kitty keyboard protocol.
(use-package kkp
  :ensure t
  :hook (tty-setup . global-kkp-mode)
  ;; :custom
  ;; (kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  )

(use-package clipetty
  :ensure t
  :hook (tty-setup . global-clipetty-mode))

(use-package ghostel
  :ensure t
  :bind (("C-x m" . ghostel)
         :map ghostel-semi-char-mode-map
         ;; I'm used to go up/down the shell history with M-n/p from eshell
         ;; Simulate this behavior in ghostel by sending C-p and C-n
         ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers)))

(use-package evil-ghostel
  :ensure t
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(provide 'cr-term)
;;; cr-term.el ends here
