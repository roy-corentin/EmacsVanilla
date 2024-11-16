;;; cr-olivetti.el --- Custom olivetti-mode setup    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: Juin 20, 2024

(use-package olivetti
  :ensure t
  :bind ("C-c o" . olivetti-mode)
  :hook (text-mode magit-mode)
  :init
  (add-hook 'magit-mode-hook (lambda () (setq-local olivetti-body-width 90))))

(defcustom cr/olivetti-width-factor 1.85
  "Factor to calculate Olivetti body width."
  :type 'float
  :group 'olivetti)

(defcustom cr/olivetti-min-width 110
  "Minimum Olivetti body width."
  :type 'integer
  :group 'olivetti)

(defcustom cr/olivetti-target-modes '(prog-mode dired-mode conf-mode)
  "Major to activate cr/olivetti-on-large-prog-window-mode"
  :type 'list
  :group 'olivetti)

(defun cr/--olivetti-body-width ()
  "Calculate the optimal Olivetti body width based on frame width."
  (max (floor (/ (frame-width) cr/olivetti-width-factor)) cr/olivetti-min-width))

(defun cr/window--olivetti-condition ()
  "Determine if Olivetti mode should be enabled based on window width."
  (>= (window-width) olivetti-body-width))

(defun cr/refresh-olivetti-body-width ()
  "Refresh Olivetti body width and toggle mode if necessary."
  (when (derived-mode-p cr/olivetti-target-modes)
    (cr/set-olivetti-body-width)
    (cr/olivetti-on-large-prog-window)))

(defun cr/set-olivetti-body-width ()
  (when olivetti-mode
    (olivetti-mode 0))
  (let ((new-width (cr/--olivetti-body-width)))
    (setq-default olivetti-body-width new-width)))

(defun cr/olivetti-on-large-prog-window ()
  "Toggle Olivetti mode in specific buffers based on window configuration."
  (when (derived-mode-p cr/olivetti-target-modes)
    (when olivetti-mode
      (olivetti-mode 0))
    (when (cr/window--olivetti-condition)
      (olivetti-mode 1))))

(define-minor-mode cr/olivetti-on-large-prog-window-mode
  "Toggle Olivetti mode for large programming or directory buffers."
  :lighter " CROlivettiSPW"
  :global t
  (if cr/olivetti-on-large-prog-window-mode
      (progn
	(cr/set-olivetti-body-width)
        (cr/olivetti-on-large-prog-window)
        (add-hook 'window-configuration-change-hook #'cr/refresh-olivetti-body-width)
        (add-hook 'prog-mode-hook #'cr/olivetti-on-large-prog-window)
        (add-hook 'dired-mode-hook #'cr/olivetti-on-large-prog-window))
    (olivetti-mode 0)
    (remove-hook 'window-configuration-change-hook #'cr/refresh-olivetti-body-width)
    (remove-hook 'prog-mode-hook #'cr/olivetti-on-large-prog-window)
    (remove-hook 'dired-mode-hook #'cr/olivetti-on-large-prog-window)))

(provide 'cr-olivetti)
