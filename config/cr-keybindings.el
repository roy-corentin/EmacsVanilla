;;; cr-keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 08, 2024

(require 'doom-methods)

(defun cr/vterm-in-project (arg)
  "Open a terminal buffer in the current window at project root.
If prefix ARG is non-nil, cd into `default-directory' instead of project root.
Returns the vterm buffer."
  (interactive "P")
  (let ((default-directory (if arg default-directory (project-root (project-current t)))))
    (setenv "PROOT" default-directory)
    (vterm vterm-buffer-name)))

(defun +default/search-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
         (if arg
             (read-directory-name "Search directory: ")
           default-directory)))
    (call-interactively #'consult-ripgrep)))

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

;; Create a new tab, switch to the project and rename the tab with project name
(defun cr/switch-project-in-new-tab ()
  (interactive)
  (tab-new)
  (call-interactively #'project-switch-project)
  (tab-rename (project-name (project-current))))

(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (add-hook 'org-agenda-mode-hook #'general-override-local-mode)
  ;; set up 'SPC' as the global leader key
  (general-create-definer cr/leader-keys
    :states '(normal insert visual motion emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode
  (cr/leader-keys
    "." '(find-file :which-key "Find file")
    "SPC" '(project-find-file :which-key "Find file in project")
    "b" '(:ignore t :which-key "Buffer")
    "b b" '(consult-project-buffer :which-key "Switch project buffer")
    "b B" '(consult-buffer :which-key "Switch all buffer")
    "b i" '(ibuffer :which-key "Ibuffer")
    "b k" '(kill-current-buffer :which-key "Kill current buffer")
    "b r" '(rename-buffer :which-key "Rename current buffer")
    "f" '(:ignore t :which-key "File")
    "f f" '(find-file :which-key "Find file")
    "f r" '(consult-recent-file :which-key "Recentf")
    "f R" '(rename-file :which-key "Recent files")
    "f s" '(save-buffer :which-key "Save file")
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
    "w m" '(delete-other-windows :which-key "Maximize window")
    "g" '(:ignore t :which-key "Git")
    "g g" '(magit-status :which-key "Magit Status")
    "p" '(:ignore t :which-key "Project")
    "p p" '(cr/switch-project-in-new-tab :which-key "Switch project")
    "p d" '(project-dired :which-key "Dired project")
    "p e" '(project-eshell :which-key "Eshell project")
    "p c" '(project-compile :which-key "Compile project")
    "f" '(:ignore t :which-key "Find")
    "f r" '(recentf :which-key "Recent files")
    "f p" '(cr/find-config-file :which-key "Recent files")
    "f t" '(tab-bar-select-tab-by-name :which-key "Tab")
    "n" '(:ignore t :which-key "Note")
    "n f" '(cr/find-note :which-key "Find note")
    "n r" '(:ignore t :which-key "Roam")
    "n r f" '(org-roam-node-find :which-key "Find roam note")
    "t" '(:ignore t :which-key "Toggle")
    "t t" '(global-tab-line-mode :which-key "Tab Line")
    "t w" '(toggle-frame-tab-bar :which-key "Tab Bar")
    "t o" '(olivetti-mode :which-key "Olivetti")
    "o" '(:ignore t :which-key "Open")
    "o p" '(treemacs :which-key "Treemacs")
    "o A" '(org-agenda :which-key "Org-Agenda")
    "o T" '(cr/vterm-in-project :which-key "Open Vterm here")
    "TAB" '(:ignore t :which-key "Tab")
    "TAB TAB" '(tab-list :which-key "List tabs")
    "TAB n" '(tab-new :which-key "New tab")
    "TAB l" '(tab-next :which-key "Next tab")
    "TAB h" '(tab-previous :which-key "Previous tab")
    "TAB d" '(tab-close :which-key "Close tab")
    "TAB D" '(tab-close-other :which-key "Close other tabs")
    "TAB r" '(tab-rename :which-key "Rename tab")
    "TAB s" '(tab-switch :which-key "Switch tab")
    "h" '(:ignore t :which-key "Help")
    "h f" '(describe-function :which-key "Describe function")
    "h F" '(describe-face :which-key "Describe face")
    "h v" '(describe-variable :which-key "Describe variable")
    "h m" '(describe-mode :which-key "Describe mode")
    "h k" '(describe-key :which-key "Describe key")
    "h K" '(describe-keymap :which-key "Describe keymap")
    "h o" '(describe-font :which-key "Describe font")
    "h c" '(describe-char :which-key "Describe char")
    "h t" '(consult-theme :which-key "Load theme")
    "s" '(:ignore t :which-key "Search")
    "s s" '(consult-line :which-key "Search in file")
    "s i" '(consult-imenu :which "IMenu")
    "s p" '(consult-ripgrep :wich "Search in Project")
    "s e" '(consult-flymake :which "Search Erros")
    "s d" '(+default/search-cwd :which "Search in current dir")
    "s u" '(vundo :which "Vundo")
    "d" '(:ignore t :which-key "Dired")
    "d d" '(dired-jump :which-key "Dired here")
    "i" '(:ignore t :which-key "Insert")
    "i y" '(consult-yank-pop :which-key "Yanks")
    "c" '(:ignore t :which-key "Code")
    "c d" '(evil-goto-definition :which-key "Go to definition")
    "c D" '(xref-find-references :which-key "Find References")
    "c a" '(eglot-code-actions :which-key "Find References"))
  "c r" '(eglot-rename :which-key "Eglot rename")
  ;; evil-multiedit
  (general-define-key
   :states 'normal
   "M-d" 'evil-multiedit-match-symbol-and-next
   "M-D" 'evil-multiedit-match-symbol-and-prev)
  (general-define-key
   :states 'visual
   "R" 'evil-multiedit-match-all
   "M-d" 'evil-multiedit-match-and-next
   "M-D" 'evil-multiedit-match-and-prev)
  (general-define-key
   :states '(visual normal)
   "C-M-d" 'evil-multiedit-restore)
  (general-define-key
   :states '(normal insert)
   :keymaps 'evil-multiedit-state-map
   "M-d" 'evil-multiedit-match-and-next
   "M-S-d" 'evil-multiedit-match-and-prev
   "RET" 'evil-multiedit-toggle-or-restrict-region
   "C-n" 'evil-multiedit-next
   "C-p" 'evil-multiedit-prev) 
  )

(use-package drag-stuff
  :ensure t
  :bind (("C-M-k" . drag-stuff-up)
         ("C-M-j" . drag-stuff-down)))

(provide 'cr-keybindings)
;;; cr-keybindings.el ends here
