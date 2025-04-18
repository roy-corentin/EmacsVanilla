;;; cr-org.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 10, 2024

(use-package mixed-pitch
  :ensure t
  :hook text-mode)

(use-package org
  :ensure nil
  :defer t
  :preface
  (defvar custom-org-roam-daily-directory "~/Dropbox/RoamNotes/daily")
  (defvar variable-pitch-font (face-attribute 'variable-pitch :family))
  (defvar fixed-pitch-font (face-attribute 'fixed-pitch :family))
  (defun cr/set-pretty-symbol ()
    "Beautify Org Checkbox Symbol"
    (push '("[ ]" .  "☐") prettify-symbols-alist)
    (push '("[X]" . "☑" ) prettify-symbols-alist)
    (push '("[-]" . "❍" ) prettify-symbols-alist)
    (push '("[C]" . "󰅘" ) prettify-symbols-alist)
    (push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
    (push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
    (push '("#+begin_src" . "↦" ) prettify-symbols-alist)
    (push '("#+end_src" . "⇤" ) prettify-symbols-alist)
    (push '("#+BEGIN_EXAMPLE" . "↦" ) prettify-symbols-alist)
    (push '("#+END_EXAMPLE" . "⇤" ) prettify-symbols-alist)
    (push '("#+begin_example" . "↦" ) prettify-symbols-alist)
    (push '("#+end_example" . "⇤" ) prettify-symbols-alist)
    (push '("#+BEGIN_QUOTE" . "↦" ) prettify-symbols-alist)
    (push '("#+END_QUOTE" . "⇤" ) prettify-symbols-alist)
    (push '("#+begin_quote" . "󱆧" ) prettify-symbols-alist)
    (push '("#+end_quote" . "󱆨⇤" ) prettify-symbols-alist)
    (push '("#+TITLE:" . "") prettify-symbols-alist)
    (push '("#+title:" . "") prettify-symbols-alist)
    (push '("#+DESCRIPTION:" . "󰦨") prettify-symbols-alist)
    (push '("#+ID:" . "") prettify-symbols-alist)
    (push '("#+FILETAGS:" . "") prettify-symbols-alist)
    (push '("#+filetags:" . "") prettify-symbols-alist)
    (push '("#+STARTUP:" . "󰈈") prettify-symbols-alist)
    (push '("#+startup:" . "󰈈") prettify-symbols-alist)
    (push '("#+ACTIVE:" . "") prettify-symbols-alist)
    (push '("#+START_SPOILER" . "") prettify-symbols-alist)
    (push '("#+CLOSE_SPOILER" . "") prettify-symbols-alist)
    (push '("#+BEGIN_HIDDEN" . "󰘓") prettify-symbols-alist)
    (push '("#+END_HIDDEN" . "󰘓") prettify-symbols-alist)
    (push '("#+author:" . "") prettify-symbols-alist)
    (push '("#+AUTHOR:" . "") prettify-symbols-alist)
    (push '("#+property:" . "") prettify-symbols-alist)
    (push '("#+PROPERTY:" . "") prettify-symbols-alist)
    (prettify-symbols-mode t))
  (defun cr/org-font-setup ()
    (setq-local display-line-numbers-type 'visual)
    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.3)
                    (org-level-2 . 1.2)
                    (org-level-3 . 1.1)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.0)
                    (org-level-6 . 1.0)
                    (org-level-7 . 1.0)
                    (org-level-8 . 1.0)))
      (set-face-attribute (car face) nil :font variable-pitch-font :weight 'medium :height (cdr face)))
    ;; Make the document title bigger
    (set-face-attribute 'org-document-title nil :font variable-pitch-font :weight 'bold :height 2.1)
    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-todo nil :family fixed-pitch-font)
    (set-face-attribute 'org-done nil :family fixed-pitch-font)
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-checkbox-statistics-todo nil :family fixed-pitch-font)
    (set-face-attribute 'org-checkbox-statistics-done nil :family fixed-pitch-font))
  (defface my-org-emphasis-bold
    '((default :inherit bold)
      (((class color) (min-colors 88) (background light))
       :foreground "#a60000")
      (((class color) (min-colors 88) (background dark))
       :foreground "#ff8059"))
    "My bold emphasis for Org."
    :group 'org-faces)
  :hook (org-mode . cr/org-font-setup)
  :hook (org-after-todo-statistics . cr/org-summary-todo)
  :hook (org-mode . cr/set-pretty-symbol)
  :custom
  (org-capture-templates '(("t" "Todo" entry (file+headline "~/org/todos.org" "Tasks")
                            "* TODO %?\n  %i\n  %a")
                           ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
                            "* %?\nEntered on %U\n  %i\n  %a")
                           ("c" "Contacts" entry (file "~/org/contacts.org")
                            "* %(org-contacts-template-name)\n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:END:")))
  (org-directory "~/Dropbox/Org/")
  (org-agenda-files (list org-directory custom-org-roam-daily-directory))
  (org-agenda-file-regexp "\\`[^.].*todo\\.org\\'")
  (org-agenda-start-with-log-mode t)
  (org-fontify-done-headline t)
  (org-hide-leading-stars t)
  (org-log-done 'time)
  (org-ellipsis " ▼ ")
  (org-log-into-drawer t)
  (org-startup-indented t)
  (org-image-actual-width nil)
  (org-startup-with-inline-images t)
  (org-startup-with-latex-preview t)
  (org-enforce-todo-dependencies t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-tags-column 0)
  (org-M-RET-may-split-line nil)
  (org-insert-heading-respect-content nil)
  (org-default-priority 67)
  (org-list-allow-alphabetical t)
  (org-hierarchical-todo-statistics nil)
  (mixed-pitch-mode t)
  (org-priority-faces '((?A . error)
                        (?B . warning)
                        (?C . success)))
  (org-emphasis-alist '(("*" my-org-emphasis-bold)
                        ("/" italic)
                        ("_" underline)
                        ("=" org-verbatim)
                        ("~" org-code)
                        ("+" (:strike-through t))))
  (org-todo-keyword-faces
   '(("WIP" . (:foreground "#b7a1f5")) ("HOLD" . org-default)
     ("[ ]" . (:inherit org-todo :weight bold)) ("[-]" . (:foreground "#b7a1f5"))
     ("[?]" . org-default)
     ("👷🏻WIP" . org-tag) ("🔒HOLD" . org-default)))
  (org-hide-emphasis-markers t)
  (org-todo-keywords
   '((sequence
      "TODO(t)"       ; A task that is ready to be tackled
      "WIP(i)"        ; A task that is in progress
      "|"             ; The pipe necessary to separate "active" states and "inactive" states
      "DONE(d)"       ; Task has been completed
      "HOLD(h)"       ; Something is holding up this task
      "CANCELED(c)" ) ; Task has been canceled
     (sequence
      "🚩TODO(f)"     ; A task that is ready to be tackled
      "👷🏻WIP(w)"      ; A task that is in progress
      "|"             ; The pipe necessary to separate "active" states and "inactive" states
      "✔DONE(e)"      ; Task has been completed
      "🔒HOLD(l)"     ; Something is holding up this task
      "❌CANCELED(x)" )
     (sequence
      "[ ](T)"        ; A task that is ready tobe tackled
      "[-](I)"        ; A task that is already started
      "|"             ; The pipe necessary to separate "active" states and "inactive" states
      "[X](D)"
      "[?](H)"        ; A task that is holding up by a reason ?
      "[C](C)")))
  (org-agenda-custom-commands
   '(("c" "Simple agenda view"
      ((tags-todo "+PRIORITY=\"A\""
                  ((org-agenda-overriding-header "High-priority unfinished tasks:")))
       (agenda "" ((org-agenda-prefix-format "%-15T\t%s%l [ ] ")
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-start-on-weekday nil)
                   (org-deadline-warning-days 60)
                   (org-agenda-start-day "0d")
                   (org-agenda-start-with-log-mode nil)
                   (org-agenda-skip-scheduled-if-deadline-is-shown t)
                   (org-agenda-log-mode-items '(state))
                   (org-agenda-overriding-header "Week Todo")))
       (agenda "" ((org-agenda-prefix-format "%-15:T\t%?-12t%l [X] ")
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'scheduled 'deadline))
                   (org-agenda-archives-mode t)
                   (org-agenda-start-day "0d")
                   (org-agenda-span 1)
                   (org-agenda-start-with-log-mode 'only)
                   (org-agenda-log-mode-items '(closed clock state))
                   (org-agenda-overriding-header "Today")))
       (agenda "" ((org-agenda-prefix-format "%-15T\t%s%l [X] ")
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'scheduled 'deadline))
                   (org-agenda-log-mode-items '(closed clock state))
                   (org-agenda-archives-mode t)
                   (org-agenda-start-day "-8d")
                   (org-agenda-span 8)
                   (org-agenda-start-with-log-mode nil)
                   (org-agenda-overriding-header "Week Done")))))
     ("d" "Done of the month"
      ((agenda "" ((org-agenda-prefix-format "%-15:T\t%t%l [X] ")
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'scheduled 'deadline))
                   (org-agenda-start-with-log-mode 'only)
                   (org-agenda-log-mode-items '(closed clock state))
                   (org-agenda-time-grid nil)
                   (org-agenda-span 31)
                   (org-agenda-start-day "-30d")
                   (org-agenda-archives-mode t)
                   (org-agenda-start-on-weekday nil)))))))
  :init
  (org-babel-do-load-languages 'org-babel-load-languages '((C . t) (ruby . t) (python . t) (shell . t) (js . t))))


(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-enable))

(use-package org-clock
  :ensure nil
  :after org
  :demand t
  :hook (kill-emacs . org-clock-save)
  :custom
  (org-clock-persist 'history)
  ;; Resume when clocking into task with open clock
  (org-clock-in-resume t)
  ;; Remove log if task was clocked for 0:00 (accidental clocking)
  (org-clock-out-remove-zero-time-clocks t)
  ;; The default value (5) is too conservative.
  (org-clock-history-length 20))

(use-package org-contacts
  :ensure t
  :after evil
  :custom
  (org-contacts-files '("~/org/contacts.org")))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory  "~/Dropbox/RoamNotes/")
  (org-roam-index-file "~/Dropbox/RoamNotes/index.org")
  (org-roam-capture-templates
   `(("d" " Default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("p" " Problems" plain
      "* [[id:f23824a1-0515-47c6-b386-21d83a9aec21][PROBLEM]]\n%?\n* SOLVING"
      :target (file+head "problems/content/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :PROBLEM:\n")
      :unnarrowed t)
     ("a", "󰙅 DataStructure" plain
      "A =${title}= [[id:92421051-83c3-4117-9c25-7f4f9ecf2c0a][Data Structure]] is %?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :DATASTRUCTURE:\n")
      :unnarrowed t)))
  (org-roam-node-display-template
   (format "${title}\t%s"
           (propertize "${tags}" 'face '(:inherit org-tag :box nil))))
  (org-roam-completion-everywhere t)
  (org-roam-list-files-commands '(fd fdfind rg find))
  :config
  (org-roam-db-autosync-enable))

(use-package websocket
  :ensure t
  :after org-roam)

(use-package org-roam-ui
  :ensure t
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

(use-package svg-tag-mode
  :ensure t
  :hook org-mode
  :preface
  (defun svg-progress-percent (value)
    (let* ((count (string-to-number value))
           (font (if (eql 100 count) 'org-checkbox-statistics-done 'org-checkbox-statistics-todo)))
      (if (zerop count)
          (svg-lib-tag (concat value "%") 'org-done :stroke 0 :margin 0)
        (svg-image (svg-lib-concat
                    (svg-lib-progress-bar (/ count 100.0)
                                          font :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                    (svg-lib-tag (concat value "%")
                                 font :stroke 0 :margin 0))
                   :ascent 'center))))
  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq)))
           (font (if (eql count total) 'org-checkbox-statistics-done 'org-checkbox-statistics-todo)))
      (if (zerop total)
          (svg-lib-tag value 'org-done :stroke 0 :margin 0)
        (svg-image (svg-lib-concat (svg-lib-progress-bar (/ count total)
                                                         font :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                                   (svg-lib-tag value
                                                font :stroke 0 :margin 0))
                   :ascent 'center))))
  :custom
  (svg-tag-tags
   '(
     ;; Org tags
     ("^#+.*\\(:[A-Z_]+:\\)" . ((lambda (tag)
                                  (svg-tag-make tag :beg 1 :end -1 :margin 1.5 :face 'org-meta-line))))
     ("^#+.*\\(:[A-Z_]+:\\)$" . ((lambda (tag)
                                   (svg-tag-make tag :beg 1 :end -1 :margin 1.5 :face 'org-meta-line))))
     ("\*.*\\(:[A-Z_]+:\\)" . ((lambda (tag)
                                 (svg-tag-make tag :beg 1 :end -1 :margin 1.5 :face 'org-tag))))
     ;; todos/dones
     ("\\(TODO\\)" . ((lambda (tag)
                        (svg-tag-make tag :inverse t :face 'org-todo))))
     ("\\(DONE\\)" . ((lambda (tag)
                        (svg-tag-make tag :inverse t :face 'org-done))))
     ("\\(WIP\\)" . ((lambda (tag)
                       (svg-tag-make tag :inverse t :face 'org-formula))))
     ("\\(HOLD\\)" . ((lambda (tag)
                        (svg-tag-make tag :inverse t :face 'org-default))))
     ("\\(CANCELED\\)" . ((lambda (tag)
                            (svg-tag-make tag :inverse t :face 'org-date))))
     ;; Progress
     ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                         (svg-progress-percent (substring tag 1 -2)))))
     ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                       (svg-progress-count (substring tag 1 -1)))))
     ;; Active date (with or without day name, with or without time)
     ("\\(<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}>\\)"
      . ((lambda (tag)
           (svg-tag-make tag :beg 1 :end -1 :margin 0))))
     ("\\(<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\)\\([A-Za-z]\\{3\\}\\.?\\)? ?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)?>"
      . ((lambda (tag)
           (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
     ("<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\(\\([A-Za-z]\\{3\\}\\.?\\)? ?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)?>\\)"
      . ((lambda (tag)
           (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))
     ;; Inactive date  (with or without day name, with or without time)
     ("\\(\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\]\\)"
      . ((lambda (tag)
           (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
     ("\\(\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\)\\([A-Za-z]\\{3\\}\\.?\\)? ?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)?\\]"
      . ((lambda (tag)
           (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
     ("\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\(\\([A-Za-z]\\{3\\}\\.?\\)? ?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)?\\]\\)"
      . ((lambda               (tag)
           (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))
     )))

(use-package consult-org-roam
  :ensure t
  :after org-roam consult-org-roam
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-."))

(use-package ox-latex
  :ensure nil
  :config
  (add-to-list 'org-latex-classes
               '("epitech"
                 "\\documentclass[a4paper,12pt]{report}
  \\renewcommand{\\chaptername}{Lab}
  \\makeatletter
  \\renewcommand{\\maketitle}{
    \\begin{titlepage}
      \\begin{center}
        \\vspace*{2em}
        \\Huge \\textbf{EPITECH} \\\\
        \\vspace{4em}
        \\Huge \\textbf{\\@title} \\\\
        \\vspace{4em}
        \\Large \\textbf{\\@date} \\\\
        \\bigskip
        \\Large \\textbf{\\@author} \\\\
        \\bigskip
        \\includegraphics[width=16em]{~/Pictures/Epitech.png} \\\\
        \\bigskip
      \\end{center}
    \\end{titlepage}
  }
  \\makeatother
  \\usepackage[margin=0.7in]{geometry}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :hook org-mode
  :custom
  (org-ai-default-chat-model "gpt-4")
  :init
  (org-ai-global-mode)
  :config
  (org-ai-install-yasnippets))

(use-package visual-fill-column
  :ensure t)

(use-package org-present
  :ensure t
  :preface
  (defun my/org-present-start (&rest args)
    (visual-line-mode 1)
    (org-display-inline-images)
    (org-present-hide-cursor)
    (org-present-read-only)
    (setq header-line-format " "))
  (defun my/org-present-end (&rest args)
    (visual-line-mode 0)
    (org-remove-inline-images)
    (org-present-show-cursor)
    (org-present-read-write)
    (setq header-line-format nil))
  (defun my/org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines
    (org-overview)
    ;; Unfold the current entry
    (org-show-entry)
    ;; Show only direct subheadings of the slide but don't expand them
    (org-show-children))
  :hook (org-present-mode . my/org-present-start)
  :hook (org-present-mode-quit . my/org-present-end)
  :custom
  (org-present-after-navigate-functions  #'my/org-present-end))

(use-package org-appear
  :ensure t
  :hook org-mode
  :preface
  (defun evil-insert-appear ()
    (add-hook 'evil-insert-state-entry-hook
              #'org-appear-manual-start
              nil
              t)
    (add-hook 'evil-insert-state-exit-hook
              #'org-appear-manual-stop
              nil
              t))
  :hook
  (org-mode . evil-insert-appear)
  :custom
  (org-appear-emphasis t)
  (org-appear-autolinks t)
  (org-appear-trigger 'manual))

(use-package org-modern-indent
  :ensure (:protocols https :inherit t :depth 1 :fetcher github :repo "jdtsmith/org-modern-indent" :files (:defaults))
  :hook org-mode)

(provide 'cr-org)
;;; cr-org.el ends here
