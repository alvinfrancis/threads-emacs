;;; lang/clojure/config.el -*- lexical-binding: t; -*-

(def-package! clojure-mode
  :mode "\\.clj$"
  :mode ("\\.cljs$" . clojurescript-mode)
  :config
  (map! :map clojure-mode-map
        [remap eval-last-sexp]       #'cider-eval-last-sexp
        [remap eval-print-last-sexp] #'cider-eval-print-last-sexp
        [remap eval-defun]           #'cider-eval-defun-at-point
        [remap eval-region]          #'cider-eval-region
        [remap eval-buffer]          #'cider-eval-buffer
        (:localleader
          :n "c" #'cider-jack-in
          :n "b" #'cider-swith-to-repl-buffer)))


(def-package! cider
  ;; NOTE: if you don't have an org directory set (the dir doesn't exist), cider jack in won't work.
  :commands (cider-jack-in cider-mode cider-jack-in-clojurescript)
  :config
  (setq nrepl-hide-special-buffers t)

  ;; settings for cider repl as a popup (prevent it from being closed on escape, especially.)
  (set! :popup "^\\*cider" :regexp t :noselect t :noesc t)

  ;; Setup cider for clojurescript / figwheel dev.
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))"))
