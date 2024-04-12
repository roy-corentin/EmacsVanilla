;;; cr-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 10, 2024
;;;

(use-package treemacs
  :ensure t
  :defer t
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc)
  :config
  ;; Add ignored files and file extensions
  (setq treemacs-file-ignore-extensions '("o" "gcna" "gcdo" "vscode" "idea")
        treemacs-file-ignore-globs nil)
  (defun my-treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions' and globs."
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (cl-loop for glob in treemacs-file-ignore-globs
                 thereis (file-name-match-glob glob full-path))))
  (add-to-list 'treemacs-ignored-file-predicates #'my-treemacs-ignore-filter)
  ;; Set treemacs theme
  (setq doom-themes-treemacs-theme "doom-colors"))

(use-package treemacs-nerd-icons
  :ensure t
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil
  :ensure t
  :after treemacs)

(use-package treemacs-magit
  :ensure t
  :after treemacs magit)

(provide 'cr-treemacs)
