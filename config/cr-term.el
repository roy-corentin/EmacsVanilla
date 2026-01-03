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
  ;; :custom
  ;; (kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  :init
  (global-kkp-mode +1))

(use-package clipetty
  :ensure t
  :hook (tty-setup . global-clipetty-mode))

(provide 'cr-term)
;;; cr-term.el ends here
