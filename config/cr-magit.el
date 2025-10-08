;;; cr-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 13, 2024

;;; Commentary:


;;; Code:

(use-package cond-let
  :ensure (:host github :repo "tarsius/cond-let")
  :defer t)

(use-package ghub
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :after nerd-icons
  :defer t
  :bind ("C-x g" . magit-status)
  :custom
  (magit-blame-echo-style 'headings)
  (magit-auto-revert-mode nil)
  (magit-refresh-status-buffer nil)
  (transient-default-level 5)
  (magit-diff-refine-hunk t) ; show granular diffs in selected hunk
  ;; Don't autosave repo buffers. This is too magical, and saving can
  ;; trigger a bunch of unwanted side-effects, like save hooks and
  ;; formatters. Trust the user to know what they're doing.
  (magit-save-repository-buffers nil)
  ;; Don't display parent/related refs in commit buffers; they are rarely
  ;; helpful and only add to runtime costs.
  (magit-revision-insert-related-refs nil)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  :config
  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))

(use-package forge
  :ensure t
  :after magit
  :defer t
  :custom
  (forge-add-default-bindings nil)
  :config
  (add-to-list 'forge-alist '("gitlab.mesvaccins.net" "gitlab.mesvaccins.net/api/v4" "gitlab.mesvaccins.net" forge-gitlab-repository)))

(provide 'cr-magit)
;;; cr-magit.el ends here
