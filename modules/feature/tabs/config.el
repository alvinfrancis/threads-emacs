;;; feature/tabs/config.el -*- lexical-binding: t; -*-

(def-memoized! +tabs--make-xpm (color height width)
  "Create an XPM bitmap."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (create-image
      (concat
       (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
               (length (car data))
               (length data)
               color
               color)
       (apply #'concat
              (cl-loop with idx = 0
                       with len = (length data)
                       for dl in data
                       do (cl-incf idx)
                       collect
                       (concat "\""
                               (cl-loop for d in dl
                                        if (= d 0) collect (string-to-char " ")
                                        else collect (string-to-char "."))
                               (if (eq idx len) "\"};" "\",\n")))))
      'xpm t :ascent 'center))))

(defvar +tabs-height 29)

(defvar +tabs-width 3)

(def-package! tab-bar
  :init
  (defun +tab-bar/tab-name ()
    (let* ((buffer-name (concat " "
                                (buffer-name (window-buffer (minibuffer-selected-window)))
                                " ")))
      (concat
       (when (display-graphic-p)
         (+tabs--make-xpm
          nil ;; no color as this is only used to extend the height and width of tabs
          +tabs-height
          +tabs-width))
       buffer-name)))

  (setq tab-bar-close-button-show nil
        tab-bar-format '(tab-bar-format-history
                         tab-bar-format-tabs
                         tab-bar-separator)
        tab-bar-show 1
        tab-bar-separator ""
        tab-bar-new-tab-to 'rightmost)
  (customize-set-variable 'tab-bar-tab-name-function #'+tab-bar/tab-name)


  :config
  (defalias #'+tabs/display #'toggle-tab-bar-mode-from-frame)
  (defalias #'+tabs/new #'tab-bar-new-tab)
  (defalias #'+tabs/close #'tab-bar-close-tab)
  (defalias #'+tabs/switch-left #'tab-bar-switch-to-prev-tab)
  (defalias #'+tabs/switch-right #'tab-bar-switch-to-next-tab)
  (tab-bar-mode t))
