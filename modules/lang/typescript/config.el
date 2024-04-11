;;; lang/typescript/config.el -*- lexical-binding: t; -*-

(def-package! typescript-mode
  :mode "\\.ts\\'"
  :init
  (setq typescript-indent-level 2)
  :config
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)
  (after! yasnippet
    (add-hook 'typescript-mode-hook #'yas-minor-mode-on))
  (after! lsp-mode
    (add-hook 'typescript-mode-hook #'lsp-deferred))

  (set! :electric 'typescript-mode :chars '(?\} ?\)) :words '("||" "&&"))

  )
