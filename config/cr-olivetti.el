;;; cr-olivetti.el --- Olivetti setup                -*- lexical-binding: t; -*-

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

(use-package olivetti
  :ensure t
  :preface
  (defun set-flymake-fringes-indicator()
    (setq-local flymake-indicator-type 'fringes))
  (defun set-flymake-margins-indicator()
    (setq-local flymake-indicator-type 'margins))
  :hook (olivetti-mode-on . set-flymake-fringes-indicator)
  :hook (olivetti-mode-off . set-flymake-margins-indicator)
  :hook (text-mode magit-mode)
  :hook (magit-mode . (lambda () (setq-local olivetti-body-width fill-column)))
  :custom
  (olivetti-mode-on-hook nil))

(provide 'cr-olivetti)
;;; cr-olivetti.el ends here
