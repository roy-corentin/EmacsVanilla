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

(use-package nerd-icons
  :ensure t)

(use-package cr-kithar
  :after olivetti
  :config
  (kithar-mode t))

(use-package which-key
  :ensure nil
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-secondary-delay 0.05)
  (which-key-popup-type 'side-window)
  :config
  (push '(("" ."\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil .  "◂\\1")) which-key-replacement-alist)
  (which-key-mode))

(use-package elec-pair
  :preface
  (defun disable-arrow-pair ()
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))
  (defun disable-parenthesis-pair ()
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (char-equal c ?\() t (,electric-pair-inhibit-predicate c)))))
  :hook (org-mode . disable-arrow-pair)
  :hook (minibuffer-mode . disable-parenthesis-pair))

(use-package ace-window
  :ensure t
  :defer t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'global aw-background t)
  :init
  (global-set-key [remap other-window] #'ace-window))

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package keycast
  :ensure t
  :defer t)

(use-package verb
  :ensure t)

(use-package posframe
  :ensure t)

(use-package tab-bar
  :after dashboard
  :custom
  (tab-bar-show t)
  :init
  (advice-add #'tab-new :after #'dashboard-open))

(use-package helpful
  :ensure t)

(use-package pgmacs
  :after pg
  :ensure (:protocols https :inherit t :depth 1 :fetcher github :repo "emarsden/pgmacs", :files (:defaults)))

(use-package casual
  :ensure t)

(use-package tramp
  :config
  ;; Enable full-featured Dirvish over TRAMP on certain connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
                     "direct-async-process" t))
  :custom
  ;; Tips to speed up connections
  (tramp-verbose 0)
  (tramp-chunksize 2000)
  (tramp-use-ssh-controlmaster-options nil))

(use-package reader
  :ensure (:host "codeberg" :repo "divyaranjan/emacs-reader" :files ("reader.el" "render-core.so") :pre-build ("make" "all")))

(use-package dumb-jump
  :ensure t
  :hook prog-mode
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'ag))

(use-package helix
  :ensure (:host github :repo "mgmarlow/helix-mode"))

(require 'cr-methods)
(require 'cr-org)
(require 'cr-term)
(require 'cr-buffer)
(require 'cr-olivetti)
(require 'cr-magit)
(require 'cr-yasnippet)
(require 'cr-eglot)
(require 'cr-project)
(require 'cr-dashboard)
(require 'cr-completion)
(require 'cr-undo)
(require 'cr-treesit)
(require 'cr-formatter)
;;(require 'cr-meow)
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

;;; init.el ends here
