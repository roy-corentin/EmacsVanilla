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
   '(".project"))
  )

(provide 'cr-project)
