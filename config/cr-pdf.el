;;; cr-pdf.el --- pdf package setup                  -*- lexical-binding: t; -*-

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

(use-package pdf-tools
  :ensure t
  ;; :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package reader
  :ensure (:host "codeberg" :repo "divyaranjan/emacs-reader" :files ("*.el" "render-core.dylib") :pre-build ("make" "all"))
  :mode "\\.pdf\\'"
  :defer t)

(provide 'cr-pdf)
;;; cr-pdf.el ends here
