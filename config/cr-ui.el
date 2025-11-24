;;; cr-ui.el --- Ui setup                            -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Roy Corentin

;; Author: Roy Corentin <croy@motherbase-xps139340>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(use-package nerd-icons
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :disabled t
  :hook prog-mode)

(use-package rainbow-mode
  :ensure t
  :defer t)

(use-package vim-tab-bar
  :ensure (:host github :repo "jamescherti/vim-tab-bar.el")
  :config
  (vim-tab-bar-mode))

(use-package indent-bars
  :ensure t
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-starting-column 1)
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                       if_statement with_statement while_statement)
                               (ruby module class method)
                               (tsx export_statement interface_declaration class_declaration
                                    method_definition function_declaration for_statement
                                    if_statement while_statement try_statement type_alias_declaration
                                    lexical_declaration jsx_element pair call_expression)
                               (typescript export_statement interface_declaration class_declaration
                                           method_definition function_declaration for_statement
                                           if_statement while_statement try_statement type_alias_declaration
                                           lexical_declaration pair call_expression)
                               (c compound_statement)))
  :hook ((python-base-mode yaml-mode ruby-base-mode typescript-ts-base-mode c-ts-mode zig-ts-mode) . indent-bars-mode))

(use-package spacious-padding
  :ensure t
  :custom
  (spacious-padding-subtle-mode-line t)
  (spacious-padding-widths
   '(
     :internal-border-width 15
     :header-line-width 4
     :mode-line-width 6
     :tab-width 4
     :right-divider-width 30
     :scroll-bar-width 8
     :fringe-width 4
     ))
  :config
  (spacious-padding-mode))

(use-package diff-hl
  :ensure t
  ;; :hook (dired-mode . diff-hl-dired-mode) ; HACK uncomment if you don't use dirvish
  :hook (magit-pre-refresh . diff-hl-magit-pre-refresh)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (diff-hl-bmp-max-width 4)
  (diff-hl-disable-on-remote t)
  (vc-git-diff-switches '("--histogram"))
  (diff-hl-flydiff-delay 0.5)
  (diff-hl-update-async t)
  (diff-hl-draw-borders nil)
  ;; UX: get realtime feedback in diffs after staging/unstaging hunks.
  (diff-hl-show-staged-changes nil)
  :config
  (global-diff-hl-mode t))

(use-package hl-todo
  :ensure t
  :hook prog-mode
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   '(;; For reminders to change or add something at a later date.
     ("TODO" warning bold)
     ;; For code (or code paths) that are broken, unimplemented, or slow,
     ;; and may become bigger problems later.
     ("FIXME" error bold)
     ;; For code that needs to be revisited later, either to upstream it,
     ;; improve it, or address non-critical issues.
     ("REVIEW" font-lock-keyword-face bold)
     ;; For code smells where questionable practices are used
     ;; intentionally, and/or is likely to break in a future update.
     ("HACK" font-lock-constant-face bold)
     ;; For sections of code that just gotta go, and will be gone soon.
     ;; Specifically, this means the code is deprecated, not necessarily
     ;; the feature it enables.
     ("DEPRECATED" font-lock-doc-face bold)
     ;; Extra keywords commonly found in the wild, whose meaning may vary
     ;; from project to project.
     ("NOTE" success bold)
     ("BUG" error bold)
     ("XXX" font-lock-constant-face bold))))

(use-package goggles
  :ensure t
  :hook (prog-mode text-mode)
  :custom
  (goggles-pulse t)) ;; set to nil to disable pulsing

(use-package buffer-box
  :ensure (:host github :repo "rougier/buffer-box"))

(provide 'cr-ui)
;;; cr-ui.el ends here
