;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

;; Load the package manager and initialize MELPA

;; (use-package compile-angel
;;   :ensure (:protocol https :depth 3 :inherit t  :fetcher github :repo "jamescherti/compile-angel.el" :files (:defaults))
;;   :demand t
;;   :hook (emacs-lisp-mode . compile-angel-on-save-local-mode)
;;   :custom
;;   (compile-angel-verbose nil)
;;   :config
;;   (compile-angel-on-load-mode))

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

(use-package apheleia
  :ensure t
  :config
  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))
  (apheleia-global-mode +1))

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

(use-package copilot
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "copilot-emacs/copilot.el" :files (:defaults))
  :defer t
  :bind (:map copilot-completion-map
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion)))

(use-package keycast
  :ensure t
  :defer t)

(use-package verb
  :ensure t)

(use-package load-env-vars
  :ensure t
  :config
  (load-env-vars (concat user-emacs-directory ".env")))

(use-package posframe
  :ensure t)

(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

(use-package tab-bar
  :ensure nil
  :after dashboard
  :custom
  (tab-bar-show t)
  :init
  (advice-add #'tab-new :after #'dashboard-open))

(use-package ultra-scroll
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "jdsmith/ultra-scroll" :files (:defaults))
  :custom
  (scroll-conservatively 101) ; important !
  (scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(require 'cr-org)
(require 'cr-term)
(require 'cr-global-keybindings)
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
(require 'cr-treemacs)
;;(require 'cr-meow)
(require 'cr-evil)
(require 'cr-dired)
(require 'cr-language)
(require 'cr-debugger)
(require 'cr-gnuplot)
(require 'cr-compile)
(require 'cr-flymake)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "d317d3dacbd7b00340d86dd00c3353bed527c722c4b034fed580d6cdafac0d96"
     "11feb87b02688866cef2199e268cad5f6d473ebacaa5f06c35c3ac08894a2845"
     "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6"
     "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-diagnostic-tag-unnecessary-face ((t (:foreground "grey"))))
 '(eglot-highlight-symbol-face ((t (:inherit highlight :weight bold))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
