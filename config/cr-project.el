;;; cr-project.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 07, 2024
;; Modified: avril 07, 2024
;;;

;;; Commentary:

;;; Code:

(use-package project
  :ensure nil
  :custom
  (project-vc-ignores '("target/" "obj/" "node_modules/" "_build/"))
  (project-vc-extra-root-markers '(".project"))
  (project-switch-commands '((project-find-file "Find file" ?f)
                             (project-find-regexp "Find regexp" ?r)
                             (project-find-dir "Find directory" ?d)
                             (cr/vterm-buffer "Vterm" ?v)
                             (cr/magit-in-project "Magit" ?m))))

(provide 'cr-project)
;;; cr-project.el ends here
