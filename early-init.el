;;; early-init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)
;; Ensure JIT compilation is enabled for improved performance by
;; native-compiling loaded .elc files asynchronously
(setq native-comp-jit-compilation t)

(setq package-enable-at-startup nil)
;; Initialize load path for loading configuration files
(add-to-list 'load-path (concat user-emacs-directory "config/"))

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(use-package emacs
  :ensure nil
  :preface
  (defun enable-show-trailing-whitespace ()
    (setq show-trailing-whitespace t))
  (defun get-or-create-dashboard-buffer ()
    (get-buffer-create dashboard-buffer-name))
  (defun reset-text-scale ()
    (interactive)
    (text-scale-set 0))
  :hook
  (prog-mode . display-line-numbers-mode)
  (yaml-ts-mode . display-line-numbers-mode)
  (org-mode . display-line-numbers-mode)
  (prog-mode . enable-show-trailing-whitespace)
  :custom
  (use-short-answers t)
  (native-comp-async-report-warnings-errors nil)
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)
  (fill-column 80)
  (inhibit-startup-screen t)
  (initial-buffer-choice #'get-or-create-dashboard-buffer)
  (column-number-mode t)
  (display-line-numbers-type 'relative)
  (display-line-numbers-width 3)
  (display-line-numbers-current-absolute t)
  (indent-tabs-mode nil)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (tab-always-indent 'complete)
  (grep-use-headings t)
  (completion-eager-display nil)
  (vc-follow-symlinks nil)
  (kill-buffer-quit-windows t)
  (mouse-autoselect-window t)
  (focus-follow-mouse t)
  (ibuffer-use-header-line 'title)
  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :custom-face
  (default ((t :family "Iosevka Nerd Font" :weight medium :height 120)))
  (fixed-pitch ((t :family "Iosevka Nerd Font" :weight bold :height 120)))
  (variable-pitch ((t :family "Iosevka Nerd Font" :weight bold :height 120)))
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (delete-selection-mode 1)
  (global-hl-line-mode 1)
  (tab-bar-mode 1)
  (desktop-save-mode 1)
  (set-default 'truncate-lines t)
  (pixel-scroll-mode t)
  (pixel-scroll-precision-mode t)
  (electric-pair-mode t)
  (set-fringe-mode '(4 . 4))
  (global-auto-revert-mode t)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file)
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  :bind
  ("C-=" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("C-," . previous-buffer)
  ("C-;" . next-buffer)
  ("C-+" . reset-text-scale)
  ("C-M-=" . global-text-scale-adjust))

(use-package gcmh
  :ensure t
  :if (not (fboundp 'igc-stats)) ; Disabled gcmh if emacs compiled with igc
  :config
  (gcmh-mode 1))

(provide 'early-init)
