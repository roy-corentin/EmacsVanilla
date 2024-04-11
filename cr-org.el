;;; cr-org.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 10, 2024

(defvar variable-pitch-font "C059")
(defvar fixed-pitch-font "JetBrains Mono Nerd Font")

(use-package mixed-pitch
  :ensure t
  :hook (org-mode . mixed-pitch-mode))

(defun cr/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font variable-pitch-font :weight 'medium :height (cdr face)))

  ;; Make the document title bigger
  (set-face-attribute 'org-document-title nil :inherit 'fixed-pitch :weight 'bold :height 2.1)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-todo nil :family fixed-pitch-font)
  (set-face-attribute 'org-done nil :family fixed-pitch-font)
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox-statistics-todo nil :family fixed-pitch-font)
  (set-face-attribute 'org-checkbox-statistics-done nil :family fixed-pitch-font))

(defun cr/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries of a TODO are done, to TODO otherwise."
  (let ((org-log-done org-todo-log-states)
        (todo-state (org-get-todo-state)))   ; turn off logging
    (when (member todo-state org-todo-keywords-1)
      (org-todo (if (= n-not-done 0) "DONE" "TODO")))))


(use-package org
  :ensure nil
  :init
  (add-hook 'org-mode-hook #'cr/org-font-setup)
  (add-hook 'org-after-todo-statistics-hook #'cr/org-summary-todo)
  (setq org-directory "~/Dropbox/Org/"
        org-agenda-files (list org-directory)
        org-log-done 'time
        org-log-into-drawer t
        org-startup-indented t
        org-image-actual-width nil
        org-startup-with-inline-images t
        org-startup-with-latex-preview t
        org-enforce-todo-dependencies t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-tags-column 0
        org-M-RET-may-split-line nil
        org-insert-heading-respect-content t
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        mixed-pitch-mode t)
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"             ; A task that is ready to be tackled
           "IN-PROGRESS(i)"      ; A task that is in progress
           "HOLD(h)"             ; Something is holding up this task
           "|"                   ; The pipe necessary to separate "active" states and "inactive" states
           "DONE(d)"             ; Task has been completed
           "CANCELED(c)" )      ; Task has been canceled
          (sequence
           "🚩TODO(f)"           ; A task that is ready to be tackled
           "👷🏻IN-PROGRESS(w)"    ; A task that is in progress
           "🔒HOLD(l)"           ; Something is holding up this task
           "|"                   ; The pipe necessary to separate "active" states and "inactive" states
           "✔DONE(e)"           ; Task has been completed
           "❌CANCELED(x)" )
          (sequence
           "[ ](T)"               ; A task that is ready tobe tackled
           "[-](I)"               ; A task that is already started
           "[?](H)"               ; A task that is holding up by a reason ?
           "|"                    ; The pipe necessary to separate "active" states and "inactive" states
           "[X](D)" )))
  )

(use-package toc-org
  :ensure t
  :after org
  :hook (org-mode . toc-org-enable))

(use-package org-clock
  :ensure nil
  :config
  (setq org-clock-persist 'history
        ;; Resume when clocking into task with open clock
        org-clock-in-resume t
        ;; Remove log if task was clocked for 0:00 (accidental clocking)
        org-clock-out-remove-zero-time-clocks t
        ;; The default value (5) is too conservative.
        org-clock-history-length 20)
  (add-hook 'kill-emacs-hook #'org-clock-save))

;; (use-package org-eldoc
;;   :ensure nil
;;   :hook (org-mode . org-eldoc-load)
;;   :init (setq org-eldoc-breadcrumb-separator " → ")
;;   :config
;;   ;; HACK Fix infinite recursion when eldoc kicks in 'org' or 'python'
;;   ;;   src blocks.
;;   (puthash "org" #'ignore org-eldoc-local-functions-cache))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Dropbox/RoamNotes")
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
      :unnarrowed t))))

(provide 'cr-org)
;;; cr-org.el ends here
