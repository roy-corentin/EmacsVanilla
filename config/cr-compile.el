;;; cr-compile.el --- Compile configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 10, 2024

(defun run-with-typescript ()
  "Set the default compil-command to run the current typescript project"
  (setq-local compile-command
              (cond ((file-exists-p (concat (project-root(project-current)) "pnpm-lock.yaml")) "pnpm tsc")
                    ((file-exists-p (concat (project-root(project-current)) "package-lock.json")) "npm run tsc")
                    ((file-exists-p (concat (project-root(project-current)) "bun.lockb")) "bun tsc"))))

(defun run-with-python ()
  "Set the default compil-command to run the current file with python"
  (setq-local compile-command
              (concat "python3 "
                      (when buffer-file-name
                        shell-quote-argument buffer-file-name))))


(use-package compile
  :ensure nil
  :config
  (push 'bun-test compilation-error-regexp-alist)
  (push '(bun-test "\sat\s\\([a-zA-Z0-9/\\._-]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)
        compilation-error-regexp-alist-alist)
  (add-hook 'typescript-ts-mode-hook 'run-with-typescript)
  (add-hook 'python-ts-mode-hook 'run-with-python))

(provide 'cr-compile)
;;; cr-compile.el ends here
