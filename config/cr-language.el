;;; cr-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: mai 01, 2024

(use-package zig-mode
  :ensure t)

(use-package crystal-mode
  :ensure t)

(use-package csv
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package docker
  :ensure t)

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

(provide 'cr-language)
