;;; cr-keybindings.el --- Setup of keybindings       -*- lexical-binding: t; -*-

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

(bind-key "C-c f n" 'cr/find-note)
(bind-key "C-x x k" 'kill-current-buffer)
(bind-key "C-x p s" 'consult-grep)
(bind-key "C-x b" 'cr/project-buffer-dwim)
(bind-key "C-c s" 'consult-line)
(bind-key "C-q" 'kill-emacs)
(bind-key "C-x p r" 'project-recompile)

(bind-key "C-c c r" 'eglot-rename)

(bind-key "C-c o l A" 'gptel-agent)

(provide 'cr-keybindings)
;;; cr-keybindings.el ends here
