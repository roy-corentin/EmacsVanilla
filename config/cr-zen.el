;;; cr-zen.el --- Zen display                        -*- lexical-binding: t; -*-

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

(define-minor-mode zen-mode
  "Global minor mode combining `buffer-box, nano-vertico.
nano-modeline hooks, and spacious-padding."
  :global t
  :lighter " Zen"
  (if zen-mode
      (progn
        (buffer-box-on)
        (nano-vertico-mode 1)
        (spacious-padding-mode 1)
        (doom-modeline nil)
        (add-hook 'text-mode-hook            #'buffer-box-on)
        (add-hook 'prog-mode-hook            #'buffer-box-on)
        (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
        (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
        (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
        (add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
        (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
        (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
        (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
        (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode))
    (buffer-box-off)
    (nano-vertico-mode nil)
    (spacious-padding-mode -1)
    (doom-modeline t)
    (remove-hook 'text-mode-hook            #'buffer-box-on)
    (remove-hook 'prog-mode-hook            #'buffer-box-on)
    (remove-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
    (remove-hook 'text-mode-hook            #'nano-modeline-text-mode)
    (remove-hook 'org-mode-hook             #'nano-modeline-org-mode)
    (remove-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
    (remove-hook 'term-mode-hook            #'nano-modeline-term-mode)
    (remove-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
    (remove-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
    (remove-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)))

(provide 'cr-zen)
;;; cr-zen.el ends here
