;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! cider)
(when (featurep! :feature syntax-checker)
  (package! flycheck-clj-kondo))
