;;; -*- lexical-binding: t -*-

;; To prevent "Defining as dynamic an already lexical var" from +vertico/embark-preview
;;;###autoload
(defvar embark-quit-after-action)

;; From Doom
;;;###autoload
(defun +evil/window-split-and-follow ()
  "Split current window horizontally, then focus new window.
If `evil-split-window-below' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-split-window-below (not evil-split-window-below)))
    (call-interactively #'evil-window-split)))

;; From Doom
;;;###autoload
(defun +evil/window-vsplit-and-follow ()
  "Split current window vertically, then focus new window.
If `evil-vsplit-window-right' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
    (call-interactively #'evil-window-vsplit)))

;; From Doom
;;;###autoload
(defun +vertico/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (if (fboundp 'embark-dwim)
        (save-selected-window
          (let (embark-quit-after-action)
            (embark-dwim)))
      (user-error "Embark not installed, aborting..."))))

(provide 'doom-methods)
