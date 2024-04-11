;;; early-init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

;; Defer package initialization
(setq package-enable-at-startup t) ;; TODO set to nil and move rest config in init.el

;; Load the package manager and initialize MELPA
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Initialize load path for loading configuration files
(add-to-list 'load-path "~/.config/emacs_vanilla/")

;; Ensure that use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load use-package
(eval-when-compile
  (require 'use-package))

(use-package emacs
  :ensure nil
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (delete-selection-mode 1)
  (global-hl-line-mode 1)
  (tab-bar-mode 1)
  (desktop-save-mode 1)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'org-mode-hook 'display-line-numbers-mode)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  :custom
  (tab-bar-show nil)
  (setq tab-bar-new-tab-choice "*dashboard*")
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)
  (fill-column 80)
  (inhibit-startup-screen t)
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  ;; (show-trailing-whitespace t)
  ;; (column-number-mode t)
  (display-line-numbers-type 'visual)
  (display-line-numbers-width 3)
  (display-line-numbers-widen t)
  (display-line-numbers-current-absolute t)
  (display-line-numbers-width-start t)
  (indent-tabs-mode nil)
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :custom-face
  (default ((t :family "JetBrains Mono Nerd Font" :height 105)))
  (fixed-pitch ((t :family "JetBrains Mono Nerd Font" :height 105)))
  (variable-pitch ((t :family "C059" :height 115)))
  :bind
  ("C-=" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("C-0" . (lambda () (interactive) (text-scale-set 0)))
  ("C-," . #'previous-buffer)
  ("C-;" . #'next-buffer)
  )

(require 'init)
