;;; cr-project.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 07, 2024
;; Modified: avril 07, 2024
;;;

(defun cr/magit-in-project ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (magit-status-setup-buffer)
    (delete-other-windows)))

(use-package project
  :ensure nil
  :custom
  (project-vc-ignores '("target/" "obj/" "node_modules/" "_build/"))
  (project-vc-extra-root-markers '(".project"))
  (project-known-project-roots)
  (project-switch-commands '((project-find-file "Find file" ?f)
                             (project-find-regexp "Find regexp" ?r)
                             (project-find-dir "Find directory" ?d)
                             (cr/vterm-buffer "Vterm" ?v)
                             (cr/magit-in-project "Magit" ?m))))

(provide 'cr-project)
