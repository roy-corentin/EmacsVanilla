;;; cr-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: mai 01, 2024

;;; Commentary:


;;; Code:

(use-package zig-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/zig-ts-mode")
  :defer t)

(use-package zig-mode
  :ensure t
  :defer t)

(use-package c3-ts-mode
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "c3lang/c3-ts-mode" :files (:defaults))
  :defer t
  :custom
  (c3-ts-mode-indent-offset 2))

(use-package crystal-mode
  :ensure t
  :defer t)

(use-package csv
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package docker
  :ensure t
  :defer t)

(use-package typescript-ts-mode
  :hook
  (tsx-ts-mode . disable-rainbow-delimiter-mode))

(use-package mhtml-ts-mode
  :hook
  (mhtml-ts-mode . disable-rainbow-delimiter-mode))

(use-package rbs-mode
  :ensure t
  :defer t)

(use-package vue-ts-mode
  :ensure (:host github :repo "8uff3r/vue-ts-mode")
  :defer t)

(use-package rspec-mode
  :ensure t
  :custom
  (rspec-use-docker-when-possible t)
  (rspec-docker-container "web")
  (rspec-docker-command "docker compose run --remove-orphans")
  :config
  (rspec-install-snippets))

(use-package csv-mode
  :ensure t
  :defer t)

(provide 'cr-language)
;;; cr-language.el ends here
