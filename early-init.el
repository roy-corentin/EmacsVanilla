;;; early-init.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

;;; Code:

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)
;; Ensure JIT compilation is enabled for improved performance by
;; native-compiling loaded .elc files asynchronously
(setq native-comp-jit-compilation t)

(setq package-enable-at-startup nil)
;; Initialize load path for loading configuration files
(add-to-list 'load-path (concat user-emacs-directory "config/"))

(defvar elpaca-installer-version 0.11)
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
  :preface
  (defun enable-show-trailing-whitespace ()
    (setq show-trailing-whitespace t))
  (defun get-or-create-dashboard-buffer ()
    (get-buffer-create dashboard-buffer-name))
  (defun reset-text-scale ()
    (interactive)
    (text-scale-set 0))
  :hook
  ((prog-mode yaml-mode org-mode) . display-line-numbers-mode)
  (prog-mode . enable-show-trailing-whitespace)
  :custom
  (use-short-answers t)
  (native-comp-async-report-warnings-errors nil)
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)
  (fill-column 90)
  (inhibit-startup-screen t)
  (initial-buffer-choice #'get-or-create-dashboard-buffer)
  (column-number-mode t)
  ;; Line numbers
  (display-line-numbers-type 'relative)
  (display-line-numbers-width 3)
  (display-line-numbers-widen t)
  (display-line-numbers-current-absolute t)
  (indent-tabs-mode nil)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (tab-always-indent 'complete)
  (grep-use-headings t)
  (completion-eager-display nil)
  (vc-follow-symlinks nil)
  ;; Mouse
  (mouse-autoselect-window t)
  (focus-follow-mouse t)
  (mouse-yank-at-point nil)
  (ibuffer-use-header-line 'title)
  ;; Corfu
  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)
  (fringes-outside-margins t)
  ;; Show paren
  (show-paren-delay 0.01)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  ;; Ring
  (kill-do-not-save-duplicates t)
  ;; Window
  (window-divider-default-places t)
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  (kill-buffer-quit-windows t)
  (window-resize-pixelwise t)
  (frame-inhibit-implied-resize t)
  ;; Scroll
  (fast-but-imprecise-scrolling t)
  ;; Hl line
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil)
  (inhibit-quit t)
  (modus-themes-mixed-fonts t)
  ;; Term
  (comint-terminfo-terminal "dumb-emacs-ansi")
  (use-package-compute-statistics t)
  :custom-face
  (default ((t :family "Iosevka Nerd Font" :weight medium :height 120)))
  (fixed-pitch ((t :family "Iosevka Nerd Font" :weight bold :height 120)))
  (variable-pitch ((t :family "Iosevka Etoile" :weight medium :height 120)))
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tab-bar-mode 1)
  (delete-selection-mode 1)
  (global-hl-line-mode -1)
  (blink-cursor-mode -1)
  (desktop-save-mode 1)
  (set-default 'truncate-lines t)
  (pixel-scroll-mode t)
  (electric-pair-mode t)
  (global-auto-revert-mode t)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file t)
  (defvar default-opacity 90)
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  (add-to-list 'default-frame-alist '(borders-respect-alpha-background . t))
  (set-face-attribute font-lock-comment-face nil :slant 'italic)
  (set-face-attribute font-lock-keyword-face nil :slant 'italic)
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

(use-package savehist
  :config
  (add-to-list 'savehist-additional-variables 'emacs-theme)
  (savehist-mode))

(use-package tab-bar
  :after dashboard
  :custom
  (tab-bar-show t)
  :init
  (advice-add #'tab-new :after #'dashboard-open))

(use-package tab-line
  :custom
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil))

(use-package which-key
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-secondary-delay 0.05)
  (which-key-popup-type 'side-window)
  :config
  (push '(("" ."\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil .  "◂\\1")) which-key-replacement-alist)
  (which-key-mode))

(require 'cr-theme)

(provide 'early-init)
;;; early-init.el ends here
