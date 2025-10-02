;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

;;; Commentary:

;; Load the package manager and initialize MELPA

;;; Code:

;; (use-package compile-angel
;;   :ensure t
;;   :demand t
;;   :hook (emacs-lisp-mode . compile-angel-on-save-local-mode)
;;   :custom
;;   (compile-angel-verbose nil)
;;   :config
;;   (compile-angel-on-load-mode))

(use-package load-env-vars
  :ensure t
  :config
  (load-env-vars (concat user-emacs-directory ".env")))

(use-package cr-kithar
  :after olivetti
  :config
  (kithar-mode t))

(use-package ace-window
  :ensure t
  :defer t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'global aw-background t)
  :init
  (global-set-key [remap other-window] #'ace-window))

(use-package keycast
  :ensure t
  :defer t)

(use-package verb
  :ensure t)

(use-package pgmacs
  :after pg
  :ensure (:protocols https :inherit t :depth 1 :fetcher github :repo "emarsden/pgmacs", :files (:defaults)))

(use-package helix
  :ensure (:host github :repo "mgmarlow/helix-mode")
  :hook ((helix-normal-mode . (lambda () (setq display-line-numbers 'relative)))
         (helix-insert-mode . (lambda () (setq display-line-numbers t)))))

(require 'cr-methods)
(require 'cr-buffer)
(require 'cr-project)
(require 'cr-snippet)
(require 'cr-eglot)
(require 'cr-ui)
(require 'cr-dashboard)
(require 'cr-completion)
(require 'cr-term)
(require 'cr-olivetti)
(require 'cr-magit)
(require 'cr-eldoc)
(require 'cr-undo)
(require 'cr-treesit)
(require 'cr-formatter)
(require 'cr-evil)
(require 'cr-dired)
(require 'cr-language)
(require 'cr-debugger)
(require 'cr-gnuplot)
(require 'cr-compile)
(require 'cr-flymake)
(require 'cr-kubernetes)
(require 'cr-scroll)
(require 'cr-ai)
(require 'cr-global-keybindings)
(require 'cr-org)
(require 'cr-elfeed)
(require 'cr-pdf)
(require 'cr-help)
(require 'cr-theme)

;;; init.el ends here
