;;; cr-org.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 10, 2024

;;; Commentary:

;;; Code:

(use-package mixed-pitch
  :ensure t
  :hook org-mode)

(use-package org
  :defer t
  :preface
  (defun cr/set-org-style ()
    (setq-local display-line-numbers-type 'visual)
    (prettify-symbols-mode t))
  (defface my-org-emphasis-bold
    '((default :inherit bold)
      (((class color) (min-colors 88) (background light))
       :foreground "#a60000")
      (((class color) (min-colors 88) (background dark))
       :foreground "#ff8059"))
    "My bold emphasis for Org."
    :group 'org-faces)
  :hook (org-mode . cr/set-org-style)
  :hook (org-after-todo-statistics . cr/org-summary-todo)
  :custom-face
  (org-level-1 ((t (:inherit variable-pitch :weight medium :height 1.3))))
  (org-level-2 ((t (:inherit variable-pitch :weight medium :height 1.2))))
  (org-level-3 ((t (:inherit variable-pitch :weight medium :height 1.1))))
  (org-level-4 ((t (:inherit variable-pitch :weight medium :height 1.0))))
  (org-level-5 ((t (:inherit variable-pitch :weight medium :height 1.0))))
  (org-level-6 ((t (:inherit variable-pitch :weight medium :height 1.0))))
  (org-level-7 ((t (:inherit variable-pitch :weight medium :height 1.0))))
  (org-level-8 ((t (:inherit variable-pitch :weight medium :height 1.0))))
  (org-document-title ((t (:weight bold :height 2.1))))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (org-block ((t (:inherit fixed-pitch))))
  (org-table ((t (:inherit fixed-pitch))))
  (org-formula ((t (:inherit fixed-pitch))))
  (org-code ((t (:inherit fixed-pitch))))
  (org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-todo ((t (:inherit fixed-pitch))))
  (org-done ((t (:inherit fixed-pitch))))
  (org-checkbox ((t (:inherit fixed-pitch))))
  (org-checkbox-statistics-todo ((t (:inherit fixed-pitch))))
  (org-checkbox-statistics-done ((t (:inherit fixed-pitch))))
  :custom
  (prettify-symbols-alist '(("#+PROPERTY:" . "") ("#+property:" . "")
                            ("#+AUTHOR:" . "") ("#+author:" . "")
                            ("#+END_HIDDEN" . "󰘓") ("#+BEGIN_HIDDEN" . "󰘓")
                            ("#+CLOSE_SPOILER" . "") ("#+START_SPOILER" . "")
                            ("#+ACTIVE:" . "󰌬")
                            ("#+filetags:" . "") ("#+FILETAGS:" . "")
                            ("#+startup:" . "󰈈") ("#+STARTUP:" . "󰈈")
                            ("#+ID:" . "")
                            ("#+DESCRIPTION:" . "󰦨")
                            ("#+title:" . "") ("#+TITLE:" . "")
                            ("#+begin_quote" . "󱆧") ("#+BEGIN_QUOTE" . "󱆧")
                            ("#+end_quote" . "󱆨") ("#+END_QUOTE" . "󱆨")
                            ("#+begin_example" . "↦") ("#+BEGIN_EXAMPLE" . "↦")
                            ("#+end_example" . "⇤") ("#+END_EXAMPLE" . "⇤")
                            ("#+begin_src" . "↦") ("#+BEGIN_SRC" . "↦")
                            ("#+end_src" . "⇤") ("#+END_SRC" . "⇤")
                            ("[C]" . "󰅘") ("[-]" . "❍") ("[X]" . "☑") ("[ ]" . "☐")))
  (org-capture-templates '(("t" "Todo" entry (file+headline "~/org/todos.org" "Tasks")
                            "* TODO %?\n  %i\n  %a")
                           ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
                            "* %?\nEntered on %U\n  %i\n  %a")
                           ("c" "Contacts" entry (file "~/org/contacts.org")
                            "* %(org-contacts-template-name)\n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:END:")))
  (org-directory "~/Dropbox/Org/")
  (org-agenda-files (list org-directory "~/Dropbox/RoamNotes/daily"))
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
                        ("=" org-verbatim verbatim)
                        ("~" org-code verbatim)
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
  :config
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
  :custom
  (org-contacts-files '("~/org/contacts.org")))

(use-package org-bullets
  :ensure t
  :hook org-mode
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
  :after org-roam ; or :after org
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
  :config
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil))
  (add-to-list 'org-latex-classes
               '("epitech_report"
                 "\\documentclass[a4paper,12pt]{report}
  \\usepackage[T1]{fontenc}
  \\usepackage{setspace}
  \\setstretch{1.5}
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
  \\pagestyle{plain}
  \\usepackage[margin=0.7in]{geometry}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package visual-fill-column
  :ensure t)

(use-package org-present
  :ensure t
  :preface
  (defun my/org-present-start (&rest args)
    (menu-bar--display-line-numbers-mode-none)
    (visual-line-mode 1)
    (org-display-inline-images)
    (org-present-hide-cursor)
    (org-present-read-only)
    (org-present-big)
    (setq header-line-format " "))
  (defun my/org-present-end (&rest args)
    (menu-bar--display-line-numbers-mode-visual)
    (visual-line-mode 0)
    (org-remove-inline-images)
    (org-present-show-cursor)
    (org-present-read-write)
    (setq header-line-format nil))
  (defun my/org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines
    (org-overview)
    ;; Unfold the current entry
    (org-fold-show-entry)
    ;; Show only direct subheadings of the slide but don't expand them
    (org-fold-show-children))
  :hook (org-present-mode . my/org-present-start)
  :hook (org-present-mode-quit . my/org-present-end)
  :custom
  (org-present-after-navigate-functions  #'my/org-present-prepare-slide))

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
