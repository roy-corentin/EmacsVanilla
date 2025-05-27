;;; cr-treesit.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 07, 2024

;;; Commentary:

;;; Code:

(use-package treesit
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
     (heex "https://github.com/phoenixframework/tree-sitter-heex")
     (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
     (make "https://github.com/alemuller/tree-sitter-make")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (c3 "https://github.com/c3lang/tree-sitter-c3")
     (zig "https://github.com/maxxnino/tree-sitter-zig")))
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesitter-context
  :ensure (:protocol https :inherit t :depth 1 :fetcher github :repo "zbelial/treesitter-context.el" :files (:defaults)))

(provide 'cr-treesit)
;;; cr-treesit.el ends here
