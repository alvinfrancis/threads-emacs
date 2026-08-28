;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode)
(package! scala-mode)
(when (featurep! :tools lsp)
  (package! lsp-metals)
  (package! lsp-docker))
