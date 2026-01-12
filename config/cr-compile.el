;;; cr-compile.el --- Compile configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 10, 2024

;;; Commentary:


;;; Code:

(use-package compile
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter)
  :hook (compilation-start . visual-line-mode)
  :custom
  (compilation-scroll-output t)
  :config
  (advice-add #'project-switch-project :after #'cr/set-compile-command)
  (push 'bun-test compilation-error-regexp-alist)
  (push '(bun-test "^\\(.*\\):\n\\([0-9]+\\)" 1 2)
        compilation-error-regexp-alist-alist)
  (push 'rspec compilation-error-regexp-alist)
  (push '(rspec "rspec\s\\([a-zA-Z0-9/\\._-]+\\):\\([0-9]+\\)" 1 2)
        compilation-error-regexp-alist-alist))

(provide 'cr-compile)
;;; cr-compile.el ends here
