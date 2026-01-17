;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

;;; Commentary:

;; Load the package manager and initialize MELPA

;;; Code:

;; Initialize load path for loading configuration files
(add-to-list 'load-path (concat user-emacs-directory "config/"))


(use-package load-env-vars
  :ensure t
  :config
  (load-env-vars (concat user-emacs-directory ".env")))

(use-package cr-kithar
  :after olivetti
  :config
  (kithar-mode t))

(use-package pgmacs
  :after pg
  :ensure (:host github :repo "emarsden/pgmacs"))

(use-package helix
  :ensure (:host github :repo "mgmarlow/helix-mode")
  :defer t
  :hook ((helix-normal-mode . (lambda () (setq display-line-numbers 'relative)))
         (helix-insert-mode . (lambda () (setq display-line-numbers t)))))

(use-package jinx
  :ensure t
  :hook text-mode)

(require 'cr-methods)
(require 'cr-ai-methods)
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
(require 'cr-undo)
(require 'cr-treesit)
(require 'cr-ediff)
(require 'cr-formatter)
(require 'cr-dired)
(require 'cr-language)
(require 'cr-debugger)
(require 'cr-gnuplot)
(require 'cr-compile)
(require 'cr-flymake)
(require 'cr-kubernetes)
(require 'cr-scroll)
(require 'cr-ai)
(require 'cr-org)
(require 'cr-elfeed)
(require 'cr-pdf)
(require 'cr-help)
(require 'cr-navigate)
(require 'cr-theme)

(provide 'init)
;;; init.el ends here
