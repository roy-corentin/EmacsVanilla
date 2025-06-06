;;; cr-compile.el --- Compile configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 10, 2024

;;; Commentary:


;;; Code:

(use-package compile
  :ensure nil
  :preface
  (defun cr/set-compile-command (&rest args)
    "Set the default compil-command to run the current project"
    (ignore args)
    (setq compile-command
          (cond ((file-exists-p (concat (project-root(project-current)) "Gemfile")) "ruby ")
                ((file-exists-p (concat (project-root(project-current)) "platformio.ini")) "platformio run -t upload")
                ((file-exists-p (concat (project-root(project-current)) "build.zig")) "zig build run")
                ((file-exists-p (concat (project-root(project-current)) "requirements.txt"))
                 (concat "python3 "
                         (when buffer-file-name
                           (shell-quote-argument buffer-file-name))))
                ((file-exists-p (concat (project-root(project-current)) "bun.lockb")) "bun tsc")
                ((file-exists-p (concat (project-root(project-current)) "pnpm-lock.yaml")) "pnpm tsc")
                ((file-exists-p (concat (project-root(project-current)) "package-lock.json")) "npm run tsc"))))
  (defun enable-visual-line (rest)
    (ignore rest)
    (visual-line-mode))
  :hook (compilation-filter . ansi-color-compilation-filter)
  :hook (compilation-start . enable-visual-line)
  :custom
  (compilation-scroll-output t)
  :init
  (advice-add #'project-switch-project :after #'cr/set-compile-command)
  (push 'bun-test compilation-error-regexp-alist)
  (push '(bun-test "\sat\s\\([a-zA-Z0-9/\\._-]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)
        compilation-error-regexp-alist-alist)
  (push 'rspec compilation-error-regexp-alist)
  (push '(rspec "rspec\s\\([a-zA-Z0-9/\\._-]+\\):\\([0-9]+\\)" 1 2)
        compilation-error-regexp-alist-alist))

(provide 'cr-compile)
;;; cr-compile.el ends here
