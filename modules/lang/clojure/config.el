;;; lang/clojure/config.el -*- lexical-binding: t; -*-

(def-package! flycheck-clj-kondo)

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
          :n "C" #'cider-connect
          :n "z" #'cider-switch-to-repl-buffer))
  (map! :map cider-repl-mode-map
        (:localleader
          :n "z" #'cider-switch-to-last-clojure-buffer)))


(def-package! cider
  ;; NOTE: if you don't have an org directory set (the dir doesn't exist), cider jack in won't work.
  :commands (cider-jack-in cider-mode cider-jack-in-clojurescript)
  :config
  (setq nrepl-hide-special-buffers t)

  (set! :jump 'cider-mode
    :definition #'cider-find-var)

  ;; settings for cider repl as a popup (prevent it from being closed on escape, especially.)
  (set! :popup "^\\*cider" :regexp t :noselect t :noesc t))
