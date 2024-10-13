;;; cr-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 13, 2024

(use-package with-editor
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "magit/with-editor" :files (:defaults) :branch "main"))

(use-package ghub
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "magit/ghub" :files (:defaults) :branch "main"))

(use-package transient
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "magit/transient" :files (:defaults) :branch "main"))

(use-package magit
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "magit/magit" :files (:defaults) :branch "main")
  :bind ("C-x g" . magit-status)
  :custom
  (magit-blame-echo-style 'headings)
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-refresh-status-buffer nil)
  (setq transient-default-level 5
        magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil)
  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))

(use-package magit-todos
  :ensure t
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (define-key magit-todos-section-map "j" nil)
  (magit-todos-mode 1))

(use-package magit-file-icons
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "gekoke/magit-file-icons" :files (:defaults))
  :after evil
  :init
  (magit-file-icons-mode 1)
  :custom
  ;; These are the default values:
  (magit-file-icons-enable-diff-file-section-icons t)
  (magit-file-icons-enable-untracked-icons t)
  (magit-file-icons-enable-diffstat-icons t))

(use-package forge
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "magit/forge" :files (:defaults) :branch "main")
  :after magit
  :custom
  (forge-add-default-bindings nil))

(provide 'cr-magit)
