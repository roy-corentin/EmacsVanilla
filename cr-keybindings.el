;;; cr-keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 08, 2024

(defun +evil/window-split-and-follow ()
  "Split current window horizontally, then focus new window.
If `evil-split-window-below' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-split-window-below (not evil-split-window-below)))
    (call-interactively #'evil-window-split)))

(defun +evil/window-vsplit-and-follow ()
  "Split current window vertically, then focus new window.
If `evil-vsplit-window-right' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
    (call-interactively #'evil-window-vsplit)))

(defun cr/find-file-in-dir(dir)
  (unless (file-directory-p dir)
    (error "Directory %S does not exist" dir))
  (unless (file-readable-p dir)
    (error "Directory %S isn't readable" dir))
  (let ((default-directory (file-truename (expand-file-name dir))))
    (call-interactively #'find-file))
  )

(defun cr/find-note ()
  (interactive)
  (unless(bound-and-true-p org-directory)
    (require 'org))
  (cr/find-file-in-dir org-directory))

(use-package general
  :ensure t
  :defer t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer cr/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (cr/leader-keys
    "." '(find-file :which-key "Find file")
    "SPC" '(project-find-file :which-key "Find file in project"))
  (cr/leader-keys
    "b" '(:ignore t :which-key "buffer")
    "b b" '(project-switch-to-buffer :which-key "Switch project buffer")
    "b B" '(switch-to-buffer :which-key "Switch all buffer")
    "b i" '(ibuffer :which-key "Ibuffer")
    "b k" '(kill-current-buffer :which-key "Kill current buffer"))
  (cr/leader-keys
    "f" '(:ignore t :which-key "file")
    "f f" '(find-file :which-key "Find file")
    "f r" '(recentf-open-files :which-key "Recent files")
    "f s" '(save-buffer :which-key "Save file"))
  (cr/leader-keys
    "w" '(:ignore t :which-key "window")
    "w h" '(evil-window-left :which-key "Move left")
    "w H" '(evil-window-move-far-left :which-key "Move window left")
    "w j" '(evil-window-down :which-key "Move down")
    "w J" '(evil-window-move-very-bottom :which-key "Move window down")
    "w k" '(evil-window-up :which-key "Move up")
    "w K" '(evil-window-move-very-top :which-key "Move window up")
    "w l" '(evil-window-right :which-key "Move right")
    "w L" '(evil-window-move-far-right :which-key "Move window right")
    "w w" '(evil-window-next :which-key "Move to next window")
    "w d" '(evil-window-delete :which-key "Delete window")
    "w s" '(evil-window-split :which-key "Split window")
    "w S" '(+evil/window-split-and-follow :which-key "Split window and follow")
    "w v" '(evil-window-vsplit :which-key "Vsplit window")
    "w V" '(+evil/window-vsplit-and-follow :which-key "Vsplit window and follow")
    "w m" '(delete-other-windows :which-key "Maximize window"))
  (cr/leader-keys
    "g" '(:ignore t :which-key "Git")
    "g g" '(magit-status :which-key "Magit Status"))
  (cr/leader-keys
    "p" '(:ignore t :which-key "Project")
    "p p" '(project-switch-project :which-key "Switch project")
    "p d" '(project-dired :which-key "Dired project")
    "p e" '(project-eshell :which-key "Eshell project"))
  (cr/leader-keys
    "f" '(:ignore t :which-key "File")
    "f r" '(recentf :which-key "Recent files"))
  (cr/leader-keys
    "n" '(:ignore t :which-key "Note")
    "n f" '(cr/find-note :which-key "Find note")
    "n r" '(:ignore t :which-key "Roam")
    "n r f" '(org-roam-node-find :which-key "Find roam note"))
  (cr/leader-keys
    "t" '(:ignore t :which-key "Toggle")
    "t t" '(treemacs :which-key "Treemacs"))
  )

(provide 'cr-keybindings)
;;; cr-keybindings.el ends here
