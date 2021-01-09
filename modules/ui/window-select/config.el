;;; ui/window-select/config.el -*- lexical-binding: t; -*-

(def-package! ace-window
  :commands (ace-window ace-swap-window ace-delete-window
                        ace-select-window ace-delete-other-windows)
  :init
  (define-key global-map [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))
