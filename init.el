;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

;; Load the package manager and initialize MELPA

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

(use-package savehist
  :ensure nil
  :config
  (add-to-list 'savehist-additional-variables 'emacs-theme)
  (savehist-mode))

(use-package olivetti
  :ensure t
  :demand t
  :bind ("C-c o" . olivetti-mode)
  :preface
  (defun force-medium-olivetti-body ()
    (setq-local olivetti-body-width 100))
  :hook (text-mode magit-mode)
  :hook (magit-mode . force-medium-olivetti-body)
  :custom
  (olivetti-body-width 110))

(use-package cr-olivetti
  :ensure nil
  :after olivetti
  :config
  (cr-olivetti-on-large-prog-window-mode t))

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
  :ensure nil
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
  :ensure nil
  :after dashboard
  :custom
  (tab-bar-show t)
  :init
  (advice-add #'tab-new :after #'dashboard-open))

(use-package helpful
  :ensure t)

(use-package ultra-scroll
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "jdsmith/ultra-scroll" :files (:defaults))
  :custom
  (scroll-conservatively 101) ; important !
  (scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

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

(require 'cr-org)
(require 'cr-term)
(require 'cr-buffer)
(require 'cr-magit)
(require 'cr-theme)
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
(require 'cr-global-keybindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6"
     "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd"
     "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" default))
 '(package-selected-packages
   '(ace-window cape corfu-terminal dape diff-hl dired-preview eglot embark-consult
                evil-anzu evil-escape evil-goggles evil-indent-plus
                evil-surround gcmh gnuplot keycast marginalia markdown-mode meow
                orderless org-contacts pdf-tools rainbow-delimiters rainbow-mode
                svg-tag-mode toc-org undo-fu undo-fu-session vertico-posframe
                vundo websocket wgrep ws-butler yasnippet zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
