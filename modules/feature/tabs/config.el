;;; feature/tabs/config.el -*- lexical-binding: t; -*-

(def-package! tab-bar
  :init
  (defun +tab-bar/tab-name ()
    (let* ((buffer-name (concat " "
                                (buffer-name (window-buffer (minibuffer-selected-window)))
                                " ")))
      (concat
       (when (display-graphic-p)
         (+doom-modeline--make-xpm
          ;; (face-background (if (window-buffer (minibuffer-selected-window))
          ;;                      'doom-modeline-bar
          ;;                    'doom-modeline-inactive-bar) nil t)
          nil
          +doom-modeline-height
          +doom-modeline-bar-width))
       buffer-name)))

  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-show 1
        tab-bar-tab-name-function #'+tab-bar/tab-name
        tab-bar-separator ""
        tab-bar-new-tab-to 'rightmost)

  :config
  (defalias #'+tabs/display #'toggle-tab-bar-mode-from-frame)
  (defalias #'+tabs/new #'tab-bar-new-tab)
  (defalias #'+tabs/close #'tab-bar-close-tab)
  (defalias #'+tabs/switch-left #'tab-bar-switch-to-prev-tab)
  (defalias #'+tabs/switch-right #'tab-bar-switch-to-next-tab)
  (tab-bar-mode t))
