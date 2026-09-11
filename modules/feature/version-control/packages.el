;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; feature/version-control/packages.el

;;; config.el
;; n/a

;;; +git
(unless (featurep! -git)
  (package! git-gutter-fringe)
  (package! git-link)
  (package! git-timemachine)
  (package! magit))

;;; TODO +hg
