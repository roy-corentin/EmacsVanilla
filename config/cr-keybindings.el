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

(bind-key "C-c f n" #'cr/find-note)
(bind-key "C-x b" #'cr/project-buffer-dwim)
(bind-key "C-x k" #'kill-current-buffer)
(bind-key "C-c s" #'consult-line)
(bind-key "C-q" #'kill-emacs)

(bind-keys :map 'project-prefix-map
           ("s" . consult-ripgrep)
           ("r" . project-recompile)
           ("e" . consult-flymake)
           ("V" . cr/project-open-file-other-window)
           ("S" . cr/project-open-file-below-window))

(bind-key "C-x t 0" #'cr/tab-close)

(bind-key "C-c c r" #'eglot-rename)
(bind-key "C-c o t" #'cr/toggle-vterm-popup)
(bind-key "C-c o T" #'cr/vterm-buffer)

(bind-key "C-c o l A" #'gptel-agent)

(use-package drag-stuff
  :ensure t
  :bind (("C-M-p" . drag-stuff-up)
         ("C-M-n" . drag-stuff-down)))

(provide 'cr-keybindings)
;;; cr-keybindings.el ends here
