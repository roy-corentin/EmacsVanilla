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

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :ensure t
  :init
  (diredfl-global-mode))

(use-package dired-open-with
  :ensure t)

(use-package casual-dired
  :ensure t
  :after dired
  :bind (:map dired-mode-map ("C-o" . #'casual-dired-tmenu)))

(provide 'cr-dired)
;;; cr-dired.el ends here
