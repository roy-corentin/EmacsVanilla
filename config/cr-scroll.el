;;; cr-scroll.el --- Scroll Config                   -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Corentin ROY

;; Author: Corentin ROY <croy@motherbase-xps139340>
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

;;; Code:

(use-package ultra-scroll
  :ensure t
  :custom
  (scroll-conservatively 0)
  (scroll-margin 0)
  :config
  (add-to-list 'ultra-scroll-hide-functions 'hl-todo-mode)
  (add-to-list 'ultra-scroll-hide-functions 'diff-hl-flydiff-mode)
  (add-to-list 'ultra-scroll-hide-functions 'jit-lock-mode)
  (add-to-list 'ultra-scroll-hide-functions 'good-scroll-mode)
  (add-to-list 'ultra-scroll-hide-functions 'indent-bars-mode)
  (ultra-scroll-mode 1))

(provide 'cr-scroll)
;;; cr-scroll.el ends here
