;;; cr-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 13, 2024

(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs '("~/.config/emacs/elpaca/repos/snippets/"
                      "~/.config/emacs/snippets/"))
  (yas-verbosity 3)
  :config
  (yas-global-mode 1))

(use-package doom-snippets
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "doomemacs/snippets" :files (:defaults))
  :after yasnippet)

(use-package auto-yasnippet
  :ensure t
  :after yasnippet
  :custom
  (aya-persist-snippets-dir (concat user-emacs-directory "snippets/")))

(use-package autoinsert
  :ensure nil
  :hook (find-file . auto-insert))

(provide 'cr-yasnippet)
