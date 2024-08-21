;;; cr-dired.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 11, 2024

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-find-file)) ; use dired-open-file or dired-find-file instead if not using dired-open package

(use-package dired
  :ensure nil
  :custom
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t))

;; (use-package nerd-icons-dired
;;   :ensure t
;;   :hook
;;   (dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :ensure t
  :init
  (diredfl-global-mode))

(use-package dired-open-with
  :ensure t)

(use-package casual-dired
  :ensure t
  :after dired)

(use-package dirvish
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "hlissner/dirvish" :files (:defaults "extensions/*"))
  :after dired
  :custom
  ;; (dirvish-reuse-session nil) ; kill all session buffers on quit
  (dirvish-use-mode-line nil)
  (dirvish-subtree-always-show-state t)
  (dirvish-attributes '(file-size collapse nerd-icons git-msg vc-state subtree-state))
  :config
  (dirvish-define-preview eza (file)
    "Use `eza' to generate directory preview."
    :require ("eza") ; tell Dirvish to check if we have the executable
    (when (file-directory-p file) ; we only interest in directories here
      `(shell . ("eza" "-laS" "--icons" "--color=always"
                 "--group-directories-first", file))))
  (add-to-list 'dirvish-preview-dispatchers 'eza)
  :init
  (dirvish-override-dired-mode))

(provide 'cr-dired)
;;; cr-dired.el ends here
