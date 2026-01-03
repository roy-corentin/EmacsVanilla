;;; cr-eldoc.el --- Eldoc setup -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Roy Corentin

;; Author: Roy Corentin <corentin.roy02@laposte.net>
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

(use-package eldoc-box
  :ensure t
  :after eglot
  :preface
  (defun cr-eldoc-box ()
    (if (display-graphic-p) (eldoc-box-hover-mode t) (eldoc-box-hover-at-point-mode t)))
  :hook (eldoc-mode . cr-eldoc-box)
  :custom
  (eldoc-box-max-pixel-height 220)
  (eldoc-box-doc-separator "\n-------\n")
  :init
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t))

(provide 'cr-eldoc)
;;; cr-eldoc.el ends here
