;;; cr-completion.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 07, 2024

;;; Commentary:


;;; Code:

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
  :init
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

(use-package eldoc-box
  :ensure t
  :after eglot
  :hook (prog-mode . eldoc-box-hover-at-point-mode)
  :init
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t))

(use-package nova
  :disabled t
  :after (vertico-posframe corfu orderless eldoc-box)
  :ensure (:host github :repo "thisisran/nova")
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
      ;; (setq-local corfu-auto nil) ; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  :hook eshell-mode
  :hook (minibuffer-setup . corfu-enable-in-minibuffer)
  ;; Optional customizations
  :bind (:map corfu-map
              ("RET" . nil)
              ("C-SPC" . corfu-insert-separator)
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous))
  :custom
  (corfu-auto t)               ; Enable auto completion
  (corfu-cycle t)              ; Enable cycling for `corfu-next/previous'
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.05)
  (corfu-popupinfo-delay '(1.0 . 1.0))
  (corfu-quit-at-boundary 'separator)
  (corfu-separator ?\s)        ; Orderless field separator
  (corfu-quit-no-match 'separator)
  (corfu-on-exact-match nil) ; Configure handling of exact matches
  (corfu-preview-current 'insert)
  (corfu-preselect 'valid)
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
  :preface
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'yasnippet-capf
                       #'cape-file))))
  :bind (("C-c p f" . cape-file)
         ("C-c p t" . complete-tag) ;; etags
         ("C-c p d" . cape-dabbrev))
  :hook
  (eglot-managed-mode . my/eglot-capf)
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package yasnippet-capf
  :ensure t
  :after cape
  :bind (("C-c p y" . yasnippet-capf))
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package orderless
  :ensure t
  :demand t
  :after vertico
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless partial-completion))
  ;; (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult
  :ensure t
  :after orderless
  :custom
  (consult-async-min-input 1))

(use-package embark
  :ensure t
  :bind
  (("C-!" . embark-act)         ;; pick some comfortable binding
   ("M-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  (embark-quit-after-action t))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t))

(use-package copilot
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "copilot-emacs/copilot.el" :files (:defaults))
  :defer t
  :bind (:map copilot-completion-map
              ("C-f" . 'copilot-accept-completion)
              ("C-<tab>" . 'copilot-accept-completion)))

(provide 'cr-completion)
;;; cr-completion.el ends here
