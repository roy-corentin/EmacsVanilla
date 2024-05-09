;;; cr-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 13, 2024

(use-package yasnippet
  :ensure t
  :defer t
  :init
  (defvar yas-verbosity 3)
  :config
  (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "elpa/snippets/"))
  (yas-global-mode 1))

(use-package auto-yasnippet 
  :ensure t
  :defer t
  :config
  (setq aya-persist-snippets-dir +snippets-dir)
  (defadvice! +snippets--inhibit-yas-global-mode-a (fn &rest args)
              "auto-yasnippet enables `yas-global-mode'. This is obnoxious for folks like
us who use yas-minor-mode and enable yasnippet more selectively. This advice
swaps `yas-global-mode' with `yas-minor-mode'."
              :around '(aya-expand aya-open-line)
              (letf! ((#'yas-global-mode #'yas-minor-mode)
                      (yas-global-mode yas-minor-mode))
                     (apply fn args))))

(use-package doom-snippets 
  :ensure nil
  :defer t) ;; TODO  use :vc when jump to Emacs30

(use-package autoinsert
  :ensure nil
  :config
  (add-hook 'find-file-hook 'auto-insert))

(provide 'cr-yasnippet)
