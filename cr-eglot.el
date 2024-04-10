;;; cr-treesit.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 10, 2024

(use-package eglot
  :ensure nil
  :hook (c-ts-mode . eglot-ensure)
  :hook (c++-ts-mode . eglot-ensure)
  :hook (python-ts-mode eglot-ensure)
  :hook (rust-ts-mode . eglot-ensure)
  :hook (typescript-ts-mode . eglot-ensure)
  :hook (tsx-ts-mode . eglot-ensure)
  :hook (ruby-ts-mode . eglot-ensure)
  :hook (json-ts-mode . eglot-ensure)
  :hook (yaml-ts-mode . eglot-ensure)
  :hook (toml-ts-mode . eglot-ensure)
  :hook (elixir-ts-mode . eglot-ensure)
  :init
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "~/Applications/ex-ls/releases/language_server.sh"))
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(provide 'cr-eglot)
