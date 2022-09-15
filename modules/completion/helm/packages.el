;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm :recipe (:fetcher github :repo "emacs-helm/helm" :commit "v3.8.5"))
(package! helm-ag)
(package! helm-c-yasnippet)
(package! helm-company)
(package! helm-css-scss)
(package! helm-describe-modes :recipe (:fetcher github :repo "emacs-helm/helm-describe-modes"))
(package! helm-projectile)
(package! helm-swoop)
(package! helm-xref)
(package! swiper)
(when (featurep! +childframe)
  (package! posframe))
