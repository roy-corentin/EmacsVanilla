;;; cr-dired.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 11, 2024

(use-package dired
  :ensure nil
  :demand t
  :preface
  (defun my-buffer-face-mode-fixed ()
    "Sets a fixed width (monospace) font in current buffer"
    (setq buffer-face-mode-face '(:family "Iosevka Nerd Font Mono" :height 120))
    (buffer-face-mode))
  :hook (dired-mode . my-buffer-face-mode-fixed)
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t))

(use-package diredfl
  :ensure t
  :after dired
  :config
  (diredfl-global-mode))

(use-package dired-open-with
  :ensure t
  :after dired)

(use-package dirvish
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "alexluigit/dirvish" :files (:defaults "extensions/*"))
  :after dired
  :requires nerd-icons
  :custom
  ;; (dirvish-reuse-session nil) ; kill all session buffers on quit
  (dirvish-use-mode-line nil)
  (dirvish-subtree-always-show-state t)
  (dirvish-attributes '(file-size collapse nerd-icons vc-state subtree-state))
  (dirvish-side-attributes '(file-size  collapse nerd-icons vc-state))
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

(use-package dired-preview
  :ensure t
  :defer t
  :preface
  (defun my-dired-preview-buffer ()
    "My preferred `dired-preview-display-action-alist-function'."
    '((display-buffer-reuse-window)
      (window . main)))
  :custom
  (dired-preview-delay 0.2)
  (dired-preview-display-action-alist #'my-dired-preview-buffer))

(provide 'cr-dired)
;;; cr-dired.el ends here
