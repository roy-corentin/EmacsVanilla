;;; cr-project.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 07, 2024
;; Modified: avril 07, 2024
;;;

(require 'project)

(use-package project
  :custom
  (project-known-project-roots)
  :config
  (setq
   project-vc-ignores
   '("target/" "bin/" "obj/" "node_modules/" "_build/")
   project-vc-extra-root-markers
   '(".project"
     "mix.exs"
     "go.mod"
     "Cargo.toml"
     "project.clj"
     "pom.xml"
     "package.json"
     "Makefile"
     "README.org"
     "README.md"))
  )

;; (defvar project-root-markers
;;   '("package.json"
;;     "tsconfig.json"
;;     "jsconfig.json"
;;     "elm.json"
;;     "mix.exs"
;;     "*.sln")
;;   "Files or directories that indicate the root of a project.")

;; ;; Probably better in .dir-locals.el.
;; (setq project-vc-ignores
;;       '("node_modules"
;;         "target"
;;         "build"
;;         "_build"
;;         "package-lock.json"
;;         "elm-stuff"))

;; (defun project-find-nomono (dir)
;;   (let ((root
;;          (locate-dominating-file
;;           dir
;;           (lambda (d)
;;             (let ((default-directory d))
;;               (seq-some #'file-expand-wildcards
;;                         project-root-markers))))))
;;     (when (and root
;;                (ignore-errors
;;                  (vc-responsible-backend root)))
;;       (cons 'vc root))))

;; (add-hook 'project-find-functions #'project-find-nomono)

(provide 'cr-project)
