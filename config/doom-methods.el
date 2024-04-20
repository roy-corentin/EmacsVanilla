;;; -*- lexical-binding: t -*-

;; To prevent "Defining as dynamic an already lexical var" from +vertico/embark-preview
;;;###autoload
(defvar embark-quit-after-action)

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
