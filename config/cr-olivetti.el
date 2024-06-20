;;; cr-olivetti.el --- Custom olivetti-mode setup    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: Juin 20, 2024

(require 'cr-methods)

(defvar cr/olivetti-on-single-prog-window-advice-list
  '(delete-window
    split-window
    find-file
    switch-to-buffer
    next-buffer
    previous-buffer
    dired)
  "List of functions to advice for `cr/olivetti-on-single-prog-window-mode'.")

(defun cr/olivetti-on-single-prog-window-enable ()
  "Enable advice for `cr/olivetti-on-single-prog-window-mode'."
  (dolist (fn cr/olivetti-on-single-prog-window-advice-list)
    (advice-add fn :after #'cr/olivetti-on-single-prog-window)))

(defun cr/olivetti-on-single-prog-window-disable ()
  "Disable advice for `cr/olivetti-on-single-prog-window-mode'."
  (dolist (fn cr/olivetti-on-single-prog-window-advice-list)
    (advice-remove fn #'cr/olivetti-on-single-prog-window)))

(define-minor-mode cr/olivetti-on-single-prog-window-mode
  "Minor mode to toggle Olivetti mode in single prog-mode or dired-mode windows."
  :lighter " CROlivettiSPW"
  :global t
  (if cr/olivetti-on-single-prog-window-mode
      (cr/olivetti-on-single-prog-window-enable)
    (cr/olivetti-on-single-prog-window-disable)))

(provide 'cr-olivetti)
