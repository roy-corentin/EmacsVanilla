;;; cr-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 13, 2024

(use-package doom-snippets
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "doomemacs/snippets" :files (:defaults))
  :defer t)

(use-package yasnippet
  :ensure t
  :defer t
  :init
  (defvar yas-verbosity 3)
  :config
  (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "elpaca/repos/snippets/"))
  (yas-global-mode 1))

(use-package auto-yasnippet
  :ensure t
  :after yasnippet
  :config
  (setq aya-persist-snippets-dir (concat user-emacs-directory "snippets/")))

(use-package autoinsert
  :ensure nil
  :config
  (add-hook 'find-file-hook 'auto-insert))

(provide 'cr-yasnippet)
