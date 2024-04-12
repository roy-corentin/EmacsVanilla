;;; cr-keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 08, 2024

;; From Doom
(defun +evil/window-split-and-follow ()
  "Split current window horizontally, then focus new window.
If `evil-split-window-below' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-split-window-below (not evil-split-window-below)))
    (call-interactively #'evil-window-split)))

;; From Doom
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
    (call-interactively #'find-file)))

(defun cr/find-config-file ()
  (interactive)
  (cr/find-file-in-dir "~/.config/emacs_vanilla/config/"))

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
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (cr/leader-keys
    "." '(find-file :which-key "Find file")
    "SPC" '(project-find-file :which-key "Find file in project"))
  (cr/leader-keys
    "b" '(:ignore t :which-key "Buffer")
    "b b" '(consult-project-buffer :which-key "Switch project buffer")
    "b B" '(consult-buffer :which-key "Switch all buffer")
    "b i" '(ibuffer :which-key "Ibuffer")
    "b k" '(kill-current-buffer :which-key "Kill current buffer"))
  (cr/leader-keys
    "f" '(:ignore t :which-key "File")
    "f f" '(find-file :which-key "Find file")
    "f r" '(recentf-open-files :which-key "Recentf")
    "f R" '(consult-recent-file :which-key "Recent files")
    "f s" '(save-buffer :which-key "Save file"))
  (cr/leader-keys
    "w" '(:ignore t :which-key "Window")
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
    "p e" '(project-eshell :which-key "Eshell project")
    "p c" '(project-compile :which-key "Compile project"))
  (cr/leader-keys
    "f" '(:ignore t :which-key "File")
    "f r" '(recentf :which-key "Recent files")
    "f p" '(cr/find-config-file :which-key "Recent files"))
  (cr/leader-keys
    "n" '(:ignore t :which-key "Note")
    "n f" '(cr/find-note :which-key "Find note")
    "n r" '(:ignore t :which-key "Roam")
    "n r f" '(org-roam-node-find :which-key "Find roam note"))
  (cr/leader-keys
    "t" '(:ignore t :which-key "Toggle")
    "t t" '(toggle-frame-tab-bar :which-key "Tab")
    "t o" '(olivetti-mode :which-key "Olivetti"))
  (cr/leader-keys
    "o" '(:ignore t :which-key "Open")
    "o p" '(treemacs :which-key "Treemacs"))
  (cr/leader-keys
    "TAB" '(:ignore t :which-key "Tab")
    "TAB TAB" '(tab-list :which-key "List tabs")
    "TAB n" '(tab-new :which-key "New tab")
    "TAB l" '(tab-next :which-key "Next tab")
    "TAB h" '(tab-previous :which-key "Previous tab")
    "TAB d" '(tab-close :which-key "Close tab")
    "TAB D" '(tab-close-other :which-key "Close other tabs")
    "TAB r" '(tab-rename :which-key "Rename tab")
    "TAB s" '(tab-switch :which-key "Switch tab"))
  (cr/leader-keys
    "h" '(:ignore t :which-key "Help")
    "h f" '(describe-function :which-key "Describe function")
    "h F" '(describe-face :which-key "Describe face")
    "h v" '(describe-variable :which-key "Describe variable")
    "h m" '(describe-mode :which-key "Describe mode")
    "h k" '(describe-key :which-key "Describe key")
    "h K" '(describe-keymap :which-key "Describe keymap")
    "h o" '(describe-font :which-key "Describe font")
    "h c" '(describe-char :which-key "Describe char")
    "h t" '(consult-theme :which-key "Load theme"))
  (cr/leader-keys
    "s" '(:ignore t :which-key "Search")
    "s s" '(consult-line :which-key "Search in file")
    "s i" '(consult-imenu :which "IMenu")
    "s p" '(consult-ripgrep :wich "Search in Project")
    "s e" '(consult-flymake :which "Search Erros"))
  (cr/leader-keys
    "d" '(:ignore t :which-key "Dired")
    "d d" '(dired-jump :which-key "Dired here"))
  (cr/leader-keys
    "i" '(:ignore t :which-key "Insert")
    "i y" '(consult-yank-pop :which-key "Yanks"))
  )

(use-package drag-stuff
  :ensure t
  :bind (("C-M-k" . drag-stuff-up)
         ("C-M-j" . drag-stuff-down)))

(provide 'cr-keybindings)
;;; cr-keybindings.el ends here
