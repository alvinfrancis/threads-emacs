;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode)

(package! lsp-ui)

(when (featurep! :completion helm)
  (package! helm-lsp))

(package! lsp-treemacs)
