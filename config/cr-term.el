;;; init.el --- Terminal Config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
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
  (vterm-max-scrollback 5000))

;; Add support for the Kitty keyboard protocol.
(use-package kkp
  :ensure t
  ;; :custom
  ;; (kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  :config
  (global-kkp-mode +1))

(provide 'cr-term)
;;; cr-term.el ends here
