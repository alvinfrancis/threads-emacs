;;; lang/scala/config.el -*- lexical-binding: t; -*-

(def-package! scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :config (setq scala-indent:align-parameters t))

(def-package! sbt-mode :after scala-mode)

;; Add metals backend for lsp-mode
(when (featurep! :feature lsp)
  (def-package! lsp-metals
    :config (setq lsp-metals-treeview-show-when-views-received t))

  ;; ;; Optional - enable lsp-mode automatically in scala files
  ;; :hook  (scala-mode . lsp)
  ;;        (lsp-mode . lsp-lens-mode)
  )
