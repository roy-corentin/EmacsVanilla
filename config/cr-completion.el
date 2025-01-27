;;; cr-completion.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 07, 2024

(use-package vertico
  :ensure t
  :demand t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              ("C-d" . vertico-scroll-up)
              ("C-u" . vertico-scroll-down)
              ("C-v" . vertico-scroll-up))
  :custom
  (vertico-cycle t)
  (vertico-count 20)
  :config
  (vertico-mode))

(use-package vertico-posframe
  :ensure t
  :after (vertico posframe)
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (vertico-posframe-height 20)
  (vertico-multiform-commands
   '((consult-ripgrep buffer (:not posframe))
     (cr/search-symbol-at-point-in-project buffer (:not posframe))
     (+default/search-cwd buffer (:not posframe))))
  :config
  (vertico-multiform-mode 1)
  (vertico-posframe-mode t))

(use-package nova
  :disabled t
  :after (vertico-posframe corfu orderless)
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "thisisran/nova" :files (:defaults))
  :custom
  (nova-vertico-depth-2-max-width 150)
  (nova-vertico-deep-depth-max-width 150)
  :config
  (nova-corfu-mode nil)
  (nova-corfu-popupinfo-mode nil)
  (nova-eldoc-mode nil)
  (nova-vertico-mode nil))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :demand t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package corfu
  :ensure t
  :preface
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  :hook eshell-mode
  :hook (minibuffer-setup . corfu-enable-in-minibuffer)
  ;; Optional customizations
  :bind (:map corfu-map
              ("C-SPC" . corfu-insert-separator)
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous))
  :custom
  (corfu-auto t)               ;; Enable auto completion
  (corfu-cycle t)              ;; Enable cycling for `corfu-next/previous'
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.18)
  (corfu-popupinfo-delay '(1.0 . 1.0))
  ;;(corfu-quit-at-boundary nil) ;; Never quit at completion boundary
  (corfu-separator ?\s)        ;; Orderless field separator
  (corfu-quit-no-match 'separator)
  ;; (corfu-preview-current nil) ;; Disable current candidate preview
  (corfu-preselect 'prompt)    ;; Preselect the prompt
  (corfu-on-exact-match 'show) ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)     ;; Use scroll margin
  :config
  (corfu-echo-mode)
  (corfu-popupinfo-mode)
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :preface
  (load (concat user-emacs-directory "config/corfu-icons.el"))
  :custom
  (nerd-icons-corfu-mapping my-corfu-icons)
  :init
  (add-to-list 'corfu-margin-formatters 'nerd-icons-corfu-formatter))

(use-package cape
  :ensure t
  :bind (("C-c p f" . cape-file)
         ("C-c p t" . complete-tag) ;; etags
         ("C-c p d" . cape-dabbrev))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package yasnippet-capf
  :ensure t
  :after cape
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package orderless
  :ensure t
  :demand t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :after orderless
  :custom
  (consult-async-min-input 1))

(use-package embark
  :ensure t
  :defer t
  :bind
  (("C-!" . embark-act)         ;; pick some comfortable binding
   ("M-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  (embark-quit-after-action t)
  :init
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ;; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t))

(provide 'cr-completion)
;;; cr-completion.el ends here
