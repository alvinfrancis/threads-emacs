;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm)
(package! helm-company)
(package! helm-css-scss)
(package! helm-describe-modes :recipe (:fetcher github :repo "emacs-helm/helm-describe-modes"))
(package! helm-projectile)
(package! helm-xref)
(package! swiper)
(when (featurep! +childframe)
  (package! posframe))
