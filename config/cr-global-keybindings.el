;;; cr-global-keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 08, 2024

(require 'doom-methods)
(require 'cr-methods)

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
    "b n" '(evil-buffer-new :which-key "Open new buffer")
    "f" '(:ignore t :which-key "File")
    "f f" '(find-file :which-key "Find file")
    "f F" '(cr/project-open-file-other-window :which-key "Find file in other window")
    "f r" '(consult-recent-file :which-key "Recentf")
    "f R" '(rename-file :which-key "Recent files")
    "f s" '(save-buffer :which-key "Save file")
    "w" '(:ignore t :which-key "Window")
    "w a" '(ace-window :which-key "Ace-window")
    "w h" '(windmove-left :which-key "Move left")
    "w H" '(windmove-swap-states-left :which-key "Move window left")
    "w j" '(windmove-down :which-key "Move down")
    "w J" '(windmove-swap-states-down :which-key "Move window down")
    "w k" '(windmove-up :which-key "Move up")
    "w K" '(windmove-swap-states-up :which-key "Move window up")
    "w l" '(windmove-right :which-key "Move right")
    "w L" '(windmove-swap-states-right :which-key "Move window right")
    "w d" '(delete-window :which-key "Delete window")
    "w D" '(delete-other-windows :which-key "Delete other windows")
    "w s" '(split-window-below :which-key "Split window")
    "w S" '(cr/split-window-below-and-follow :which-key "Split window and follow")
    "w v" '(split-window-right :which-key "Vsplit window")
    "w V" '(cr/split-window-right-and-follow :which-key "Vsplit window and follow")
    "w m" '(maximize-window :which-key "Maximize window")
    "w =" '(balance-windows :which-key "Balance window")
    "g" '(:ignore t :which-key "Git")
    "g g" '(magit-status :which-key "Magit Status")
    "g c" '(magit-clone :which-key "Magit Clone")
    "g b" '(magit-blame :which-key "Magit Blame")
    "p" '(:ignore t :which-key "Project")
    "p a" '(project-remember-projects-under :which-key "Add project")
    "p p" '(cr/switch-project-in-new-tab :which-key "Switch project")
    "p d" '(project-dired :which-key "Dired project")
    "p e" '(project-eshell :which-key "Eshell project")
    "p c" '(project-compile :which-key "Compile project")
    "p r" '(project-recompile :which-key "Recompile project")
    "f" '(:ignore t :which-key "Find")
    "f r" '(recentf :which-key "Recent files")
    "f p" '(cr/find-config-file :which-key "Recent files")
    "f t" '(tab-bar-select-tab-by-name :which-key "Tab")
    "n" '(:ignore t :which-key "Note")
    "n f" '(cr/find-note :which-key "Find note")
    "n r" '(:ignore t :which-key "Roam")
    "n r f" '(org-roam-node-find :which-key "Find roam note")
    "n r i" '(org-roam-node-insert :which-key "Insert roam note")
    "t" '(:ignore t :which-key "Toggle")
    "t a" '(apheleia-mode :which-key "Apheleia")
    "t t" '(global-tab-line-mode :which-key "Tab Line")
    "t w" '(toggle-frame-tab-bar :which-key "Tab Bar")
    "t v" '(visual-line-mode :which-key "Visual line mode")
    "t o" '(olivetti-mode :which-key "Olivetti")
    "t g" '(global-copilot-mode :which-key "Global Copilot")
    "t r" '(read-only-mode :which-key "ReadOnly mode")
    "o" '(:ignore t :which-key "Open")
    "o p" '(treemacs :which-key "Treemacs")
    "o A" '(org-agenda :which-key "Org-Agenda")
    "o t" '(cr/toggle-vterm-popup :which-key "Open Vterm in popup")
    "o T" '(cr/smart-vterm-buffer :which-key "Open Vterm in buffer")
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
    "s i" '(consult-imenu :which-key "IMenu")
    "s p" '(consult-ripgrep :wich "Search in Project")
    "s e" '(consult-flymake :which-key "Search Erros")
    "s d" '(+default/search-cwd :which-key "Search in current dir")
    "s u" '(vundo :which-key "Vundo")
    "s c" '(evil-avy-goto-char-2 :which-key "Goto char2")
    "s w" '(evil-avy-goto-word-1 :which-key "Goto word")
    "d" '(:ignore t :which-key "Dired")
    "d d" '(dired-jump :which-key "Dired here")
    "i" '(:ignore t :which-key "Insert")
    "i y" '(consult-yank-pop :which-key "Yanks")
    "c" '(:ignore t :which-key "Code")
    "c d" '(evil-goto-definition :which-key "Go to definition")
    "c D" '(xref-find-references :which-key "Find References")
    "c a" '(eglot-code-actions :which-key "Find References")
    "c r" '(eglot-rename :which-key "Eglot rename")
    "c f" '(apheleia-format-buffer :which-key "Apheleia Format Buffer")
    "q" '(:ignore t :which-key "Quit")
    "q q" '(kill-emacs :which-key "Quit Emacs")
    "q f" '(delete-frame :which-key "Delete Frame")
    "e" '(:ignore t :which-key "Eval")
    "e r" '(eval-region :which-key "Eval Region")
    "m" '(:ignore t :which-key "Local"))
  (general-define-key
   :states 'normal
   :keymaps 'ruby-ts-mode-map
   :prefix "SPC m"
   :global-prefix "M-SPC m"
   "{" '(ruby-toggle-block :which-key "Toggle block")
   "'" '(ruby-toggle-string-quotes :which-key "Toggle string quotes")
   "b" '(:ignore t :which-key "Block")
   "b b" '(ruby-beginning-of-block :which-key "Beginning of block")
   "b e" '(ruby-end-of-block :which-key "End of block")
   "f" '(:ignore t :which-key "Function")
   "f b" '(ruby-beginning-of-defun :which-key "Beginning of function")
   "f e" '(ruby-end-of-defun :which-key "End of function"))
  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   :prefix "SPC m"
   :global-prefix "M-SPC m"
   :which-key "Org Local"
   "e" '(org-export-dispatch :which-key "Org Export")
   "c" '(:ignore t :which-key "Org Clock")
   "c i" '(org-clock-in :which-key "Clock in")
   "c o" '(org-clock-out :which-key "Clock out")
   "c g" '(org-clock-goto :which-key "Clock goto")
   "t" '(org-todo :which-key "Org todo"))
  (general-define-key
   :states 'normal
   :keymaps 'csv-mode-map
   :prefix "SPC m"
   :global-prefix "M-SPC m"
   "a" '(csv-align-fields :which-key "Align fields")
   "u" '(csv-unalign-fields :which-key "Unalign fields")
   "t" '(csv-transpose :which-key "Transpose"))
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
   :keymaps 'evil-multiedit-mode-map
   "M-d" 'evil-multiedit-match-and-next
   "M-S-d" 'evil-multiedit-match-and-prev
   "RET" 'evil-multiedit-toggle-or-restrict-region
   "C-n" 'evil-multiedit-next
   "C-p" 'evil-multiedit-prev)
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   "g" '(:ignore t)
   "g =" '(org-increase-number-at-point :which-key "Increase at point")
   "g -" '(org-decrease-number-at-point :which-key "Decrease at point"))
  (general-define-key
   :states '(normal visual)
   :keympas 'dired-mode-map
   "?" 'casual-dired-tmenu)
  )

(use-package drag-stuff
  :ensure t
  :defer t
  :bind (("C-M-k" . drag-stuff-up)
         ("C-M-j" . drag-stuff-down)))

(provide 'cr-global-keybindings)
;;; cr-keybindings.el ends here
