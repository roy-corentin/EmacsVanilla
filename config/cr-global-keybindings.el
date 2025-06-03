;;; cr-global-keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 08, 2024

;;; Commentary:

;;; Code:

(use-package general
  :ensure t
  :requires (evil magit)
  :demand t
  :hook  (org-agenda-mode . general-override-local-mode)
  :config
  (general-evil-setup)
  ;; set up 'SPC' as the global leader key
  (general-create-definer cr/leader-keys
    :states '(normal insert visual motion emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode
  (cr/leader-keys
    "." '(find-file :which-key "Find file")
    "SPC" '(cr/find-file-dwim :which-key "Find file in project")
    "b" '(:ignore t :which-key "Buffer")
    "b b" '(cr/project-buffer-dwim :which-key "Switch project buffer")
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
    "w w" '(ace-window :which-key "Ace-window")
    "w W" '(ace-swap-window :which-key "Ace-swap-window")
    "w h" '(windmove-left :which-key "Move left")
    "w H" '(cr/move-window-left :which-key "Move window left")
    "w j" '(windmove-down :which-key "Move down")
    "w J" '(cr/move-window-down :which-key "Move window down")
    "w k" '(windmove-up :which-key "Move up")
    "w K" '(cr/move-window-up :which-key "Move window up")
    "w l" '(windmove-right :which-key "Move right")
    "w L" '(cr/move-window-right :which-key "Move window right")
    "w d" '(delete-window :which-key "Delete window")
    "w D" '(delete-other-windows :which-key "Delete other windows")
    "w s" '(split-window-below :which-key "Split window")
    "w S" '(cr/split-window-below-and-follow :which-key "Split window and follow")
    "w v" '(split-window-right :which-key "Vsplit window")
    "w V" '(cr/split-window-right-and-follow :which-key "Vsplit window and follow")
    "w m" '(maximize-window :which-key "Maximize window")
    "w =" '(balance-windows :which-key "Balance window")
    "w ^" '(enlarge-window :which-key "Increase window height")
    "w ¨" '(shrink-window :which-key "Decrease window height")
    "w >" '(enlarge-window-horizontally :which-key "Increase window width")
    "w <" '(shrink-window-horizontally :which-key "Decrease window width")
    "g" '(:ignore t :which-key "Git")
    "g g" '(magit-status :which-key "Magit Status")
    "g c" '(magit-clone :which-key "Magit Clone")
    "g b" '(magit-blame :which-key "Magit Blame")
    "p" '(:ignore t :which-key "Project")
    "p a" '(project-remember-projects-under :which-key "Add project")
    "p p" '(cr/switch-project-in-new-tab :which-key "Switch project")
    "p d" '(project-dired :which-key "Dired project")
    "p D" '(flymake-show-project-diagnostics :which-key "Flymake project diagnostics")
    "p f" '(project-forget-project :which-key "Forget projects")
    "p e" '(project-eshell :which-key "Eshell project")
    "p c" '(project-compile :which-key "Compile project")
    "p r" '(project-recompile :which-key "Recompile project")
    "p s" '(project-async-shell-command :which-key "Async shell command")
    "p S" '(project-shell-command :which-key "Shell command")
    "p ." '(project-root-find-file :which-key "Find file at project root")
    "f" '(:ignore t :which-key "Find")
    "f r" '(recentf :which-key "Recent files")
    "f p" '(cr/find-config-file :which-key "Recent files")
    "f t" '(tab-bar-select-tab-by-name :which-key "Tab")
    "n" '(:ignore t :which-key "Note")
    "n f" '(cr/find-note :which-key "Find note")
    "n r" '(:ignore t :which-key "Roam")
    "n r f" '(consult-org-roam-file-find :which-key "Find roam note")
    "n r i" '(org-roam-node-insert :which-key "Insert roam note")
    "t" '(:ignore t :which-key "Toggle")
    "t a" '(apheleia-mode :which-key "Apheleia")
    "t d" '(diff-hl-mode :which-key "Diff Highlights")
    "t t" '(global-tab-line-mode :which-key "Tab Line")
    "t T" '(kb/toggle-window-transparency :which-key "Transparency")
    "t w" '(toggle-frame-tab-bar :which-key "Tab Bar")
    "t v" '(visual-line-mode :which-key "Visual line mode")
    "t o" '(olivetti-mode :which-key "Olivetti")
    "t O" '(cr-olivetti-on-large-window-mode :which-key "CR Olivetti")
    "t g" '(global-copilot-mode :which-key "Global Copilot")
    "t r" '(read-only-mode :which-key "ReadOnly mode")
    "t h" '(global-hl-line-mode :which-key "Global Highlight line mode")
    "t i" '(indent-bars-mode :which-key "Indent bars mode")
    "o" '(:ignore t :which-key "Open")
    "o p" '(dirvish-side :which-key "Dirvish side")
    "o A" '(org-agenda :which-key "Org-Agenda")
    "o t" '(cr/toggle-vterm-popup :which-key "Open Vterm in popup")
    "o T" '(cr/smart-vterm-buffer :which-key "Open Vterm in buffer")
    "o l" '(:ignore t :which-key "Llm")
    "o l a" '(gptel-add :which-key "Add text to context")
    "o l e" '(gptel-quick :which-key "Explain")
    "o l f" '(gptel-add-file :which-key "Add file to context")
    "o l l" '(gptel :which-key "Open gptel")
    "o l s" '(gptel-send :which-key "Send to gptel")
    "o l m" '(gptel-menu :which-key "Open gptel menu")
    "o l r" '(gptel-rewrite :which-key "Rewrite")
    "o l o" '(gptel-org-set-topic :which-key "Org: set topic")
    "o l O" '(gptel-org-set-properties :which-key "Org: set properties")
    "o l p" '(cr/org-ai-on-current-project :which-key "Org ai on project")
    "<tab>" '(:ignore t :which-key "Tab")
    "<tab> <tab>" '(tab-list :which-key "List tabs")
    "<tab> n" '(tab-new :which-key "New tab")
    "<tab> l" '(tab-next :which-key "Next tab")
    "<tab> h" '(tab-previous :which-key "Previous tab")
    "<tab> d" '(tab-close :which-key "Close tab")
    "<tab> D" '(tab-close-other :which-key "Close other tabs")
    "<tab> r" '(tab-rename :which-key "Rename tab")
    "<tab> s" '(tab-switch :which-key "Switch tab")
    "h" '(:ignore t :which-key "Help")
    "h f" '(helpful-function :which-key "Describe function")
    "h F" '(describe-face :which-key "Describe face")
    "h v" '(helpful-variable :which-key "Describe variable")
    "h m" '(describe-mode :which-key "Describe mode")
    "h k" '(helpful-key :which-key "Describe key")
    "h K" '(describe-keymap :which-key "Describe keymap")
    "h o" '(describe-font :which-key "Describe font")
    "h c" '(describe-char :which-key "Describe char")
    "h t" '(consult-theme :which-key "Load theme")
    "s" '(:ignore t :which-key "Search")
    "s s" '(consult-line :which-key "Search in file")
    "s i" '(consult-imenu :which-key "IMenu")
    "s p" '(consult-ripgrep :wich "Search in Project")
    "s g" '(consult-git-grep :wich "Search in Git Project")
    "s e" '(consult-flymake :which-key "Search Erros")
    "s d" '(+default/search-cwd :which-key "Search in current dir")
    "s u" '(vundo :which-key "Vundo")
    "s c" '(evil-avy-goto-char-2 :which-key "Goto char2")
    "s w" '(evil-avy-goto-word-1 :which-key "Goto word")
    "d" '(:ignore t :which-key "Dired")
    "d d" '(dirvish-dwim :which-key "Dired here")
    "i" '(:ignore t :which-key "Insert")
    "i y" '(consult-yank-pop :which-key "Yanks")
    "D" '(:ignore t :which-key "Dape")
    "D d" '(dape :which-key "Dape")
    "D l" '(dape-breakpoint-log :which-key "Log breakpoint")
    "D b" '(dape-breakpoint-toggle :which-key "Breakpoint toggle")
    "D r" '(dape-restart :which-key "Restart")
    "D q" '(dape-quit :which-key "Quit")
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
    "m" '(:ignore t :which-key "Local")
    "*" '(cr/search-symbol-at-point-in-project :which-key "Search symbol at point in project"))
  (general-define-key
   :states '(visual normal)
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
   :states '(visual normal)
   :keymaps 'org-mode-map
   :prefix "SPC m"
   :global-prefix "M-SPC m"
   :which-key "Org Local"
   "e" '(org-export-dispatch :which-key "Org Export")
   "c" '(:ignore t :which-key "Org Clock")
   "c i" '(org-clock-in :which-key "Clock in")
   "c o" '(org-clock-out :which-key "Clock out")
   "c g" '(org-clock-goto :which-key "Clock goto")
   "d" '(org-deadline :which-key "Deadline")
   "s" '(org-schedule :which-key "Schedule")
   "t" '(org-todo :which-key "Org todo")
   "l" '(:ignore t :which-key "Org link")
   "l l" '(org-insert-link :which-key "Insert link")
   "S" '(svg-tag-mode :which-key "Toggle svg-tag-mode")
   "P"  '(org-present :which-key "Org present")
   "p"  '(:ignore t :which-key "priority")
   "p p" '(org-priority :which-key "priority")
   "p d" '(org-priority-down :which-key "priority down")
   "p u" '(org-priority-up :which-key "priority up"))
  (general-define-key
   :states 'normal
   :keymaps 'csv-mode-map
   :prefix "SPC m"
   :global-prefix "M-SPC m"
   :which-key "CSV Local"
   "a" '(csv-align-fields :which-key "Align fields")
   "u" '(csv-unalign-fields :which-key "Unalign fields")
   "t" '(csv-transpose :which-key "Transpose"))
  (general-define-key
   :states '(visual normal)
   :keymaps 'zig-mode-map
   :prefix "SPC m"
   :global-prefix "M-SPC m"
   :which-key "Zig Local"
   "<escape>" '(keyboard-escape-quit :which-key t)
   "b" '(zig-compile :which-key "build")
   "r" '(zig-run :which-key "run")
   "t" '(zig-test :which-key "test"))
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
   "g -" '(org-decrease-number-at-point :which-key "Decrease at point")
   "g z" '(:ignore t :which-key "Evil Multiple-Cursors")
   "g z j" '(evil-mc-make-cursor-move-next-line :which-key "Make cursor next line")
   "g z k" '(evil-mc-make-cursor-move-prev-line :which-key "Make cursor previous line")
   "g z n" '(evil-mc-make-and-goto-next-match :which-key "Make cursor next match")
   "g z N" '(evil-mc-make-and-goto-prev-match :which-key "Make cursor previous match")
   "g z a" '(evil-mc-make-all-cursors :which-key "Make cursor all match")
   "g z z" '(evil-mc-make-cursor-here :which-key "Make cursors here")
   "g z q" '(evil-mc-undo-all-cursors :which-key "Undo all cursor")
   "g z p" '(evil-mc-pause-cursors :which-key "Pause cursors")
   "g z r" '(evil-mc-resume-cursors :which-key "Resume cursors")
   "K" '(cr/eldoc-doc-buffer :which-key "Help at point"))
  (general-define-key
   :states '(normal visual)
   :keymaps 'org-mode-map
   "z" '(:ignore t)
   "z i" '(org-toggle-inline-images :which-key "Toggle inline image"))
  (general-define-key
   :states '(normal visual)
   :keymaps 'dired-mode-map
   "l" '(dired-find-file :which-key "Open file")
   "h" '(dired-up-directory :which-key "Go up in directory")
   "o" '(dired-find-file-other-window :which-key "Find file other window")
   "F" '(dired-create-empty-file :which-key "Create empty file")
   "q" 'nil
   "E" '(dired-do-open :which-key "External open")
   "?" 'casual-dired-tmenu
   "<mouse-1>" 'dirvish-subtree-toggle-or-open
   "<mouse-2>" 'dired-mouse-find-file-other-window
   "<mouse-3>" 'dired-mouse-find-file)
  (general-define-key
   :states '(normal visual)
   :keymaps 'dirvish-mode-map
   "<tab>" 'dirvish-subtree-toggle
   "s" 'dirvish-layout-switch
   "q" 'dirvish-quit)
  (general-define-key
   :keymaps 'minibuffer-mode-map
   "C-p" 'previous-history-element
   "C-n" 'next-history-element)
  (general-define-key
   :states '(normal visual)
   :keymaps 'prog-mode-map
   "<tab>" 'evil-jump-item)
  (general-define-key
   :states '(normal visual)
   :keymaps 'magit-mode-map
   "<tab>" 'magit-section-toggle
   "*"  'magit-worktree
   "zt" 'evil-scroll-line-to-top
   "zz" 'evil-scroll-line-to-center
   "zb" 'evil-scroll-line-to-bottom
   "g=" 'magit-diff)
  (general-define-key
   :states '(insert normal visual)
   :keymaps 'override
   "M-j" 'scroll-other-window
   "M-k" 'scroll-other-window-down
   "C-," 'previous-buffer
   "C-x C-;" 'cr/comment-line)
  (general-define-key
   :states '(normal)
   :keymaps 'kubernetes-mode-map
   "v" 'kubernetes-overview-set-sections
   "c" 'kubernetes-contexts-use-context
   "RET" 'kubernetes-navigate
   "l" 'kubernetes-logs)
  (general-define-key
   :keymaps 'vertico-map
   "C-SPC" '+vertico/embark-preview
   "C-M-j" #'vertico-next-group
   "C-M-k" #'vertico-previous-group
   "<wheel-down>" #'vertico-next
   "<wheel-up>" #'vertico-previous)
  (general-define-key
   :states '(normal visual)
   :keymaps 'reader-mode-map
   "j" 'reader-next-page
   "k" 'reader-previous-page
   "gg" 'reader-first-page
   "G" 'reader-last-page
   "W" 'reader-fit-to-width
   "H" 'reader-fit-to-height
   "-" 'reader-shrink-size
   "=" 'reader-enlarge-size))

(use-package drag-stuff
  :ensure t
  :bind (("C-M-k" . drag-stuff-up)
         ("C-M-j" . drag-stuff-down)))

(provide 'cr-global-keybindings)
;;; cr-global-keybindings.el ends here
