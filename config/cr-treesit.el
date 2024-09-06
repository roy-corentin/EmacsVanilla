;;; cr-treesit.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 07, 2024

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
        (heex "https://github.com/phoenixframework/tree-sitter-heex")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (latex "https://github.com/latex-lsp/tree-sitter-latex")
        (c3 "https://github.com/c3lang/tree-sitter-c3")))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package c3-ts-mode
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "c3lang/c3-ts-mode" :files (:defaults))
  :custom
  (c3-ts-mode-indent-offset 2)
  (treesit-font-lock-level 4))

(provide 'cr-treesit)
;;; cr-treesit.el ends here
