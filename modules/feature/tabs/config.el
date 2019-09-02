;;; feature/tabs/config.el -*- lexical-binding: t; -*-

(def-package! elscreen
  :init
  (setq elscreen-tab-display-control nil
        elscreen-display-tab nil
        elscreen-display-screen-number nil
        elscreen-tab-display-kill-screen nil)

  ;; (defun +elscreen/quit ()
  ;;   (interactive)
  ;;   (if (and (not (elscreen-one-screen-p))
  ;;            (one-window-p))
  ;;       (elscreen-kill)
  ;;     (evil-quit)))
  ;; (evil-ex-define-cmd "quit" #'+elscreen/quit)

  :config
  (defalias #'+tabs/display #'elscreen-toggle-display-tab)
  (defalias #'+tabs/new #'elscreen-create)
  (defalias #'+tabs/close #'elscreen-kill)
  (defalias #'+tabs/switch-left #'elscreen-previous)
  (defalias #'+tabs/switch-right #'elscreen-next)
  (elscreen-start))
