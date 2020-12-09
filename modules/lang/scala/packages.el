;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode)
(package! scala-mode)
(when (featurep! :feature lsp)
  (package! lsp-metals))
