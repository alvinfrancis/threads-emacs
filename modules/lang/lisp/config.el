;;; lang/lisp/config.el -*- lexical-binding: t; -*-

(def-package! paredit
  :commands (enable-paredit-mode)
  :init
  (add-hook! '(emacs-lisp-mode-hook
               cider-repl-mode-hook
               eval-expression-minibuffer-setup-hook
               clojure-mode-hook
               lisp-mode-hook
               lisp-interaction-mode-hook
               scheme-mode-hook)
    'enable-paredit-mode))
