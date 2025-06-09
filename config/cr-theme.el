;;; cr-keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 09, 2024

;;; Commentary:

;;; Code:

(use-package doom-themes
  :ensure t
  :demand t
  :custom
  (doom-themes-enable-bold t) ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode t))

(use-package doom-modeline
  :ensure t
  :demand t
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :hook (doom-load-theme . doom-modeline-refresh-bars)
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
  (doom-modeline-project-name t)
  :config
  (doom-modeline-mode 1))

(use-package diff-hl
  :ensure t
  :demand t
  :preface
  (defun enable-diff-hl-margin-in-tui ()
    (unless (display-graphic-p) (diff-hl-margin-local-mode)))
  ;; :hook (dired-mode . diff-hl-dired-mode) ; HACK uncomment if you don't use dirvish
  :hook (magit-pre-refresh . diff-hl-magit-pre-refresh)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :hook (diff-hl-mode-on . enable-diff-hl-margin-in-tui)
  :custom
  (diff-hl-bmp-max-width 4)
  (diff-hl-disable-on-remote t)
  (vc-git-diff-switches '("--histogram"))
  (diff-hl-flydiff-delay 0.5)
  (diff-hl-update-async t)
  ;; UX: get realtime feedback in diffs after staging/unstaging hunks.
  (diff-hl-show-staged-changes nil)
  (diff-hl-draw-borders t)
  :config
  (global-diff-hl-mode t))

(use-package git-gutter
  :ensure t
  :disabled (package-installed-p 'diff-hl)
  :hook (prog-mode text-mode)
  :custom
  (fringes-outside-margins t)
  :init
  (global-git-gutter-mode t))

(use-package hl-todo
  :ensure t
  :hook (prog-mode yaml-mode)
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
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
  :ensure t
  :demand t
  :custom
  (catppuccin-flavor 'mocha))

(use-package kaolin-themes
  :ensure t
  :demand t
  :custom
  (kaolin-themes-hl-line-colored t))

(use-package lambda-themes
  :ensure (:host github :repo "lambda-emacs/lambda-themes")
  :demand t
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t)

(use-package vim-tab-bar
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "jamescherti/vim-tab-bar.el" :files (:defaults))
  :after evil
  :config
  (vim-tab-bar-mode))

(use-package indent-bars
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "https://github.com/jdtsmith/indent-bars" :files (:defaults))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-starting-column 1)
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
                                           lexical_declaration pair call_expression)
                               (c compound_statement)))
  :hook ((python-base-mode yaml-mode ruby-base-mode typescript-ts-base-mode c-ts-mode zig-ts-mode) . indent-bars-mode)
  )

(use-package tab-line
  :ensure nil
  :custom
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil))

(use-package spacious-padding
  :ensure t
  :custom
  (spacious-padding-subtle-mode-line t)
  (spacious-padding-widths
   '(
     :internal-border-width 15
     :header-line-width 4
     :mode-line-width 6
     :tab-width 4
     :right-divider-width 30
     :scroll-bar-width 8
     :fringe-width 4
     ))
  :init
  (spacious-padding-mode 1))

(defun load-custom-theme ()
  "Load custom theme."
  (interactive)
  (load-theme (or emacs-theme 'modus-vivendi) t))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-custom-theme)
                  (set-frame-parameter frame 'alpha-background default-transparency))))
  (add-hook 'emacs-startup-hook #'load-custom-theme)
  (set-frame-parameter nil 'alpha-background default-transparency))

(provide 'cr-theme)
;;; cr-theme.el ends here
