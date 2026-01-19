;;; cr-navigate.el --- Code navigation setup         -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Roy Corentin

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

(use-package xref
  :custom
  (xref-auto-jump-to-first-definition t))

(use-package dumb-jump
  :ensure t
  :hook prog-mode
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'ag))

(use-package better-jumper
  :ensure t
  :bind (("M-o" . 'better-jumper-jump-forward)
         ("C-o" . 'better-jumper-jump-backward))
  :init
  (better-jumper-mode t))

(use-package ace-window
  :ensure t
  :defer t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'global aw-background t)
  :bind (([remap other-window] . ace-window)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(provide 'cr-navigate)
;;; cr-navigate.el ends here
