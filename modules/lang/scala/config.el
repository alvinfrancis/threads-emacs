;;; lang/scala/config.el -*- lexical-binding: t; -*-

(def-package! scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :config (setq scala-indent:align-parameters t
                scala-indent:step 2))

(def-package! sbt-mode :after scala-mode)

(def-package! lsp-metals)
