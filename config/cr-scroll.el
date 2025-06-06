;;; cr-scroll.el --- Scroll Config                   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Corentin ROY

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
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "jdsmith/ultra-scroll" :files (:defaults))
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

(use-package good-scroll
  :ensure t
  :preface
  (defun smooth-scroll--fix-out-of-bounds-error-a ()
    (save-restriction
      (widen)
      (<= (line-number-at-pos (max (point) (point-min)) t)
          (1+ (line-number-at-pos (min (window-start) (point-max)) t)))))
  (defun good-scroll--convert-line-to-step (line)
    (cond ((integerp line) (* line (line-pixel-height)))
          ((or (null line) (memq '- line))
           (- (good-scroll--window-usable-height)
              (* next-screen-context-lines (line-pixel-height))))
          ((line-pixel-height))))
  (defun good-scroll--scroll-up (fn &optional arg)
    (if good-scroll-mode
        (good-scroll-move (good-scroll--convert-line-to-step arg))
      (funcall fn arg)))
  (defun good-scroll--scroll-down (fn &optional arg)
    (if good-scroll-mode
        (good-scroll-move (- (good-scroll--convert-line-to-step arg)))
      (funcall fn arg)))
  (defun smooth-scroll-coexist-with-ultra-scroll-h ()
    (when good-scroll-mode
      (setq mwheel-scroll-up-function #'scroll-up
            mwheel-scroll-down-function #'scroll-down)))
  :hook
  (good-scroll-mode . smooth-scroll-coexist-with-ultra-scroll-h)
  :custom
  (good-scroll-render-rate 0.05)
  :config
  ;; (good-scroll-mode 1)
  (advice-add #'scroll-up :around #'good-scroll--scroll-up)
  (advice-add #'scroll-down :around #'good-scroll--scroll-down)
  (advice-add #'good-scroll--point-at-top-p :override #'smooth-scroll--fix-out-of-bounds-error-a))

(provide 'cr-scroll)
;;; cr-scroll.el ends here
