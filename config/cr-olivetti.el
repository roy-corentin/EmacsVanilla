;;; cr-olivetti.el --- Custom olivetti-mode setup    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: Juin 20, 2024

;;; Commentary:


;;; Code:

(defcustom cr-olivetti-width-factor 2
  "Factor to calculate Olivetti body width."
  :type 'float
  :group 'olivetti)

(defcustom cr-olivetti-min-width 110
  "Minimum Olivetti body width."
  :type 'integer
  :group 'olivetti)

(defcustom cr-olivetti-target-modes '(prog-mode dired-mode conf-mode)
  "Major to activate `cr-olivetti-on-large-window-mode'."
  :type '(list symbol)
  :group 'olivetti)

(defun cr--olivetti-body-width ()
  "Calculate the optimal Olivetti body width based on frame width."
  (max (floor (/ (frame-width) cr-olivetti-width-factor)) cr-olivetti-min-width))

(defun cr--window--olivetti-condition ()
  "Determine if Olivetti mode should be enabled based on window width."
  (>= (window-width) olivetti-body-width))

(defun cr-refresh-olivetti ()
  "Refresh Olivetti body width and toggle mode if necessary."
  (when (derived-mode-p cr-olivetti-target-modes)
    (cr--set-olivetti-body-width)
    (cr-olivetti-on-large-window)))

(defun cr--set-olivetti-body-width ()
  "Update value `olivetti-body-width'."
  (when olivetti-mode
    (olivetti-mode 0))
  (let ((new-width (cr--olivetti-body-width)))
    (setq-default olivetti-body-width new-width)))

(defun cr-olivetti-on-large-window ()
  "Toggle Olivetti mode in specific buffers based on window configuration."
  (when (derived-mode-p cr-olivetti-target-modes)
    (when olivetti-mode
      (olivetti-mode 0))
    (when (cr--window--olivetti-condition)
      (olivetti-mode 1)
      (visual-line-mode 0))))

(define-minor-mode cr-olivetti-on-large-window-mode
  "Toggle Olivetti mode for large programming or directory buffers."
  :lighter "CROlivettiLPW"
  :group 'olivetti
  :global t
  :require 'cr-olivetti
  (if cr-olivetti-on-large-window-mode
      (progn
	(cr--set-olivetti-body-width)
        (cr-olivetti-on-large-window)
        (add-hook 'window-state-change-hook #'cr-refresh-olivetti))
    (olivetti-mode 0)
    (remove-hook 'window-state-change-hook #'cr-refresh-olivetti)))

(provide 'cr-olivetti)
;;; cr-olivetti.el ends here
