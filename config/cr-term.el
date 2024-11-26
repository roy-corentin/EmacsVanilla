;;; init.el --- Terminal Config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

(use-package vterm
  :ensure t
  :defer t
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000))

;; Add support for the Kitty keyboard protocol.
(use-package kkp
  :ensure t
  :defer t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

(provide 'cr-term)
