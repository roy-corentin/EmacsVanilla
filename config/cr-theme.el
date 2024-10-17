;;; cr-keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 09, 2024

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-moonlight t)
  ;; (load-theme 'modus-vivendi-tinted t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package ewal
  :ensure t
  :custom
  (ewal-use-built-in-on-failure-p t))

(use-package ewal-doom-themes
  :ensure t
  :after ewal
  :init
  (load-theme 'catppuccin t))

(use-package solaire-mode
  :ensure t
  :init
  (solaire-global-mode t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :custom
  (doom-modeline-bar-width 3)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-position-column-line-format '("%l:%c"))
  (doom-modeline-minor-modes nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-workspace-name nil)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-mode 1)
  :config
  (add-hook 'doom-load-theme-hook #'doom-modeline-refresh-bars))

;; (use-package git-gutter
;;   :ensure t
;;   :hook (prog-mode text-mode)
;;   :custom
;;   (fringes-outside-margins t)
;;   :init
;;   (global-git-gutter-mode t)
;;   :config
;;   (fringe-mode '8))

(use-package diff-hl
  :ensure t
  ;; :hook (dired-mode . diff-hl-dired-mode) ;; HACK uncomment if you don't use dirvish
  :custom
  (diff-hl-disable-on-remote t)
  (vc-git-diff-switches '("--histogram"))
  ;; PERF: Slightly more conservative delay before updating the diff
  (diff-hl-flydiff-delay 0.5)  ; default: 0.3
  ;; PERF: don't block Emacs when updating vc gutter
  (diff-hl-update-async t)
  ;; UX: get realtime feedback in diffs after staging/unstaging hunks.
  (diff-hl-show-staged-changes nil)
  :init
  (global-diff-hl-mode t)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'diff-hl-mode-on-hook (lambda () (unless (display-graphic-p) (diff-hl-margin-local-mode)))))

(use-package hl-todo
  :ensure (:protocol https :depth 1 :inherit t  :fetcher github :repo "tarsius/hl-todo" :version (lambda (_) "3.8.1" ) :files (:defaults))
  :hook (prog-mode yaml-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))))

(use-package catppuccin-theme
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode))

(use-package rainbow-mode
  :ensure t)

(use-package vim-tab-bar
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "jamescherti/vim-tab-bar.el" :files (:defaults))
  :init
  (add-hook 'after-init-hook #'vim-tab-bar-mode))

(use-package indent-bars
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "https://github.com/jdtsmith/indent-bars" :files (:defaults))
  :config
  (require 'indent-bars-ts) ; not needed with straight
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	                               if_statement with_statement while_statement)
                               (ruby module class method call if)
                               (tsx export_statement interface_declaration class_declaration
                                    method_definition function_declaration for_statement
                                    if_statement while_statement try_statement type_alias_declaration
                                    lexical_declaration jsx_element pair call_expression)
                               (typescript export_statement interface_declaration class_declaration
                                           method_definition function_declaration for_statement
                                           if_statement while_statement try_statement type_alias_declaration
                                           lexical_declaration pair call_expression)))
  ;; wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode ruby-base-mode typescript-ts-base-mode) . indent-bars-mode))

(use-package tab-line
  :demand t
  :custom
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil))

(provide 'cr-theme)
