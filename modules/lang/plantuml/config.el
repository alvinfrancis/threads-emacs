;;; lang/plantuml/config.el -*- lexical-binding: t; -*-

(use-package! plantuml-mode
  :defer t
  :init
  (setq plantuml-jar-path (concat doom-etc-dir "plantuml.jar")
        org-plantuml-jar-path plantuml-jar-path)
  :config
  (set-popup-rule! "^\\*PLANTUML" :size 0.4 :select nil :ttl 0))


(use-package! flycheck-plantuml
  :when (featurep! :tools flycheck)
  :after plantuml-mode
  :config (flycheck-plantuml-setup))
