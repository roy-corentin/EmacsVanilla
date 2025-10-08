;;; cr-dired.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 11, 2024

;;; Commentary:


;;; Code:

(use-package dired
  :ensure nil
  :defer t
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t))

(use-package diredfl
  :ensure t
  :after dired
  :defer t
  :hook (dired-mode dirvish-directory-view-mode)
  :config
  (diredfl-global-mode))

(use-package dired-open-with
  :ensure t
  :after dired
  :defer t)

(use-package dirvish
  :ensure t
  :requires nerd-icons
  :after dired
  :custom
  ;; (dirvish-reuse-session nil) ; kill all session buffers on quit
  (dirvish-use-mode-line nil)
  (dirvish-subtree-always-show-state t)
  (dirvish-attributes '(file-size collapse nerd-icons vc-state subtree-state))
  (dirvish-side-attributes '(file-size collapse nerd-icons vc-state))
  (dirvish-subtree-state-style 'nerd)
  (delete-by-moving-to-trash t)
  (dirvish-path-separators (list
                            (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                            (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                            (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-hide-details '(dirvish-side))
  :config
  (dirvish-define-preview eza (file)
    "Use `eza' to generate directory preview."
    :require ("eza") ; tell Dirvish to check if we have the executable
    (when (file-directory-p file) ; we only interest in directories here
      `(shell . ("eza" "-laS" "--icons=always" "--color=always"
                 "--group-directories-first" ,file))))
  (push 'eza dirvish-preview-dispatchers)
  (setq dirvish-preview-dispatchers
        (cl-substitute 'eza 'dired dirvish-preview-dispatchers))
  (dirvish-side-follow-mode)
  (dirvish-peek-mode)
  :init
  (dirvish-override-dired-mode))

(use-package nerd-icons-dired
  :ensure t
  :if (eq (fboundp 'dirvish) nil)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'cr-dired)
;;; cr-dired.el ends here
