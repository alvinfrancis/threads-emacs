;;; ui/posframe/config.el -*- lexical-binding: t; -*-

(def-package! helm-posframe
  :init
  (setq helm-posframe-poshandler #'posframe-poshandler-frame-top-center
        helm-posframe-width 200)
  :config
  (helm-posframe-enable))
