;;; cr-compile.el --- Compile configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 10, 2024

(use-package compile
  :ensure nil
  :config
  (push 'bun-test compilation-error-regexp-alist)
  (push '(bun-test "\sat\s\\([a-zA-Z0-9/\\._-]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)
        compilation-error-regexp-alist-alist)
  (add-hook 'typescript-ts-mode
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (cond ((file-exists-p (concat (project-root(project-current)) "pnpm-lock.yaml")) "pnpm tsc")
                         ((file-exists-p (concat (project-root(project-current)) "package-lock.json")) "npm tsc")
                         ((file-exists-p (concat (project-root(project-current)) "bun.lockb")) "bun tsc"))))))

(provide 'cr-compile)
;;; cr-compile.el ends here
