;;; lang/plantuml/config.el -*- lexical-binding: t; -*-

(def-package! plantuml-mode
  :mode "\\.p\\(lant\\)?uml$"
  :config
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-executable-path "/usr/bin/plantuml")
  (setq plantuml-output-type "txt")
  (set! :popup "*PLANTUML Preview*" :size 25 :noselect t :autokill t)

  (unless (file-exists-p plantuml-executable-path)
    (warn "plantuml-mode: can't find plantuml.jar; run M-x +plantuml/install.")))


(def-package! flycheck-plantuml
  :when (featurep! :feature syntax-checker)
  :after plantuml-mode
  :config (flycheck-plantuml-setup))
