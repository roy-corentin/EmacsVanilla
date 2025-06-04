;;; cr-kithar.el --- Custom olivetti-mode setup    -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: Juin 20, 2024

;;; Commentary:


;;; Code:

(defcustom kithar-width-fraction 0.5
  "Percentage of frame the window width must follow."
  :type 'float
  :group 'olivetti)

(defcustom kithar-max-width 155
  "Maximum window size tolerated."
  :type 'integer
  :group 'olivetti)

(defcustom kithar-min-width 110
  "Minimum Olivetti body width."
  :type 'integer
  :group 'olivetti)

(defcustom kithar-target-modes '(prog-mode dired-mode conf-mode)
  "Major to activate `kithar-mode'."
  :type '(list symbol)
  :group 'olivetti)

(defun kithar--body-width ()
  "Calculate the optimal Olivetti body width based on frame width."
  (max (min (floor (* (frame-width) kithar-width-fraction)) kithar-max-width) kithar-min-width))

(defun kithar-trigger ()
  "Determine if Olivetti mode should be enabled based on window width."
  (>= (window-width) kithar-max-width))

(defun kithar-refresh ()
  "Refresh Olivetti body width and toggle mode if necessary."
  (when (derived-mode-p kithar-target-modes)
    (kithar--set-body-width)
    (kithar-buffer)))

(defun kithar--set-body-width ()
  "Update value `olivetti-body-width'."
  (when olivetti-mode
    (olivetti-mode 0))
  (let ((new-width (kithar--body-width)))
    (setq-default olivetti-body-width new-width)))

(defun kithar-buffer ()
  "Toggle Olivetti mode in specific buffers based on window configuration."
  (when (and (derived-mode-p kithar-target-modes) (not (window-dedicated-p)))
    (when olivetti-mode
      (olivetti-mode 0))
    (when (kithar-trigger)
      (olivetti-mode 1))))

(define-minor-mode kithar-mode
  "Toggle Olivetti mode for large programming or directory buffers."
  ;; :lighter "Kithar"
  :global t
  :group 'kithar
  (if kithar-mode
      (progn
	(kithar--set-body-width)
        (kithar-buffer)
        (add-hook 'window-state-change-hook #'kithar-refresh))
    (olivetti-mode 0)
    (remove-hook 'window-state-change-hook #'kithar-refresh)))

(provide 'cr-kithar)
;;; cr-kithar.el ends here
