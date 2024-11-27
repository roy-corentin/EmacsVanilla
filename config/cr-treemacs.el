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
  :after doom-themes
  :preface
  (defun my-treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions' and globs."
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (cl-loop for glob in treemacs-file-ignore-globs
                 thereis (file-name-match-glob glob full-path))))
  :custom
  (treemacs-follow-after-init t)
  (treemacs-is-never-other-window nil)
  (treemacs-sorting 'alphabetic-case-insensitive-asc)
  (treemacs-file-ignore-extensions '("o" "gcna" "gcdo" "vscode" "idea"))
  (treemacs-file-ignore-globs nil)
  (doom-themes-treemacs-theme "doom-colors")
  :config
  (add-to-list 'treemacs-ignored-file-predicates #'my-treemacs-ignore-filter))

(use-package treemacs-nerd-icons
  :ensure t
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil
  :ensure t
  :after (treemacs evil))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(provide 'cr-treemacs)
