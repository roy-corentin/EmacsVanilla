;;; cr-completion.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 07, 2024

(use-package vertico
  :ensure t
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
  :after vertico posframe
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)
  :config
  (vertico-posframe-mode t))

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
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer."
  (when (local-variable-p 'completion-at-point-functions)
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))

(use-package corfu
  :ensure t
  ;; Optional customizations
  :bind (:map corfu-map
              ("C-SPC" . corfu-insert-separator)
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous))
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.18)
  (corfu-popupinfo-delay '(1.0 . 1.0))
  ;;(corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-no-match 'separator)
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'insert)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (add-hook 'eshell-mode-hook (lambda () (corfu-mode)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  (corfu-echo-mode)
  (corfu-popupinfo-mode)
  (global-corfu-mode))

(use-package corfu-terminal
  :ensure t
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(load (concat user-emacs-directory "config/corfu-icons.el"))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters 'nerd-icons-corfu-formatter)
  (setq nerd-icons-corfu-mapping my-corfu-icons))

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
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :defer t)

(use-package embark
  :ensure t
  :defer t
  :bind
  (("C-!" . embark-act)         ;; pick some comfortable binding
   ("M-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)
   ("C-e" . embark-candidate-collectors)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t)

(provide 'cr-completion)
;;; cr-completion.el ends here
