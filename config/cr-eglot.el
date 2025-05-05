;;; cr-treesit.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 10, 2024

;;; Commentary:

;;; Code:

(use-package eglot
  :ensure nil
  :hook ((c-ts-mode c3-ts-mode c++-ts-mode python-ts-mode zig-mode zig-ts-mode
                    rust-ts-mode typescript-ts-mode tsx-ts-mode ruby-ts-mode rbs-mode
                    json-ts-mode yaml-ts-mode toml-ts-mode elixir-ts-mode javascript-ts-mode
                    json-ts-mode) . eglot-ensure)
  :custom
  (eglot-connect-timeout 60)
  (eglot-advertise-cancellation t)
  (eglot-autoshutdown t)
  (eglot-code-action-indications '(eldoc-hint mode-line))
  (eglot-send-changes-idle-time 0.2)
  :custom-face
  (eglot-highlight-symbol-face ((t (:inherit highlight :weight bold))))
  :config
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls"))
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode rbs-mode) "ruby-lsp"))
  (add-to-list 'eglot-server-programs '(c3-ts-mode "c3lsp"))
  (add-to-list 'eglot-server-programs '(crystal-mode "crystalline")))

(use-package eglot-booster
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "jdtsmith/eglot-booster" :files (:defaults))
  :after eglot
  :custom
  (eglot-booster-no-remote-boost t)
  (eglot-booster-io-only nil)
  :config
  (eglot-booster-mode))

(use-package xref
  :custom
  (xref-auto-jump-to-first-definition t))

(provide 'cr-eglot)
;;; cr-eglot.el ends here
