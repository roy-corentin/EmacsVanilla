;;; cr-dired.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 11, 2024

;;; Commentary:


;;; Code:

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
              ("h" . dired-up-directory)
              ("l" . dired-find-file))
  :custom
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t))

(use-package diredfl
  :ensure t
  :after dired
  :hook dired-mode
  :config
  (diredfl-global-mode))

(use-package dired-open-with
  :ensure t
  :after dired
  :defer t)

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'cr-dired)
;;; cr-dired.el ends here
