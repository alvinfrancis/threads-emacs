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

;; (def-package! centaur-tabs
;;   :init
;;   (setq centaur-tabs-set-icons t
;;         centaur-tabs-gray-out-icons 'buffer
;;         centaur-tabs-set-bar 'left
;;         centaur-tabs-set-modified-marker t
;;         centaur-tabs-close-button "✕"
;;         centaur-tabs-modified-marker "⬤"
;;         ;; Scrolling (with the mouse wheel) past the end of the tab list
;;         ;; replaces the tab list with that of another Doom workspace. This
;;         ;; prevents that.
;;         centaur-tabs-cycle-scope 'tabs
;;         centaur-tabs--buffer-show-groups t
;;         )
;;   (defalias #'+tabs/display #'centaur-tabs-local-mode)
;;   (defalias #'+tabs/switch-left #'centaur-tabs-backward)
;;   (defalias #'+tabs/switch-right #'centaur-tabs-forward)
;;   :config
;;   (centaur-tabs-mode t))
