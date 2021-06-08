;;; lang/typescript/config.el -*- lexical-binding: t; -*-

(def-package! typescript-mode
  :mode "\\.ts\\'"
  ;; :hook (typescript-mode . lsp-deferred)
  :init
  (setq typescript-indent-level 2)
  :config
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)

  (set! :electric 'typescript-mode :chars '(?\} ?\)) :words '("||" "&&"))

  )
