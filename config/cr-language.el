;;; cr-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: mai 01, 2024

(use-package zig-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/zig-ts-mode"))

(use-package zig-mode
  :ensure t)

(use-package c3-ts-mode
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "c3lang/c3-ts-mode" :files (:defaults))
  :custom
  (c3-ts-mode-indent-offset 2))

(use-package crystal-mode
  :ensure t)

(use-package csv
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package docker
  :ensure t)

(use-package typescript-ts-mode
  :hook
  (tsx-ts-mode . disable-rainbow-delimiter-mode))

(use-package mhtml-ts-mode
  :hook
  (mhtml-ts-mode . disable-rainbow-delimiter-mode))

(use-package outline-yaml
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "jamescherti/outline-yaml.el" :files (:defaults))
  :preface
  (defun my-outline-set-global-ellipsis (ellipsis)
    "Apply the ellipsis ELLIPSIS to outline mode globally."
    (let* ((face-offset (* (face-id 'shadow) (ash 1 22)))
           (value (vconcat (mapcar (lambda (c) (+ face-offset c)) ellipsis))))
      (set-display-table-slot standard-display-table 'selective-display value)))
  :hook
  ((yaml-mode . outline-yaml-minor-mode)
   (yaml-ts-mode . outline-yaml-minor-mode))
  :init
  (my-outline-set-global-ellipsis " ▼ "))

(use-package rbs-mode
  :ensure t)

(use-package vue-ts-mode
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "8uff3r/vue-ts-mode" :files (:defaults)))

(use-package rspec-mode
  :ensure t
  :custom
  (rspec-use-docker-when-possible t)
  (rspec-docker-container "web")
  :config
  (rspec-install-snippets))

(use-package csv-mode
  :ensure t)

(provide 'cr-language)
