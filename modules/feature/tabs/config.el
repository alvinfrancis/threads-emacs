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
  (defun +tab-bar/tab-name-format (name _tab _i)
    (concat " " name " "))

  (defun +tab-bar/tab-name-format-icons (name _tab _i)
    ;; The XPM spacer must come *after* NAME: `tab-bar-auto-width' pads tabs
    ;; with (apply #'propertize " " (text-properties-at 0 name)), so a leading
    ;; XPM `display' property makes every padding char part of that same 3px
    ;; image run.  The string's pixel width then never grows and the resize
    ;; loop spins forever, hanging any (lookup-key global-map [tab-bar]) --
    ;; e.g. `evil-read-key', which is how `f'/`t' motions froze on Emacs 31.
    (concat
     name
     (when (display-graphic-p)
       (+tabs--make-xpm
        nil ;; no color as this is only used to extend the height and width of tabs
        +tabs-height
        +tabs-width))))

  (setq tab-bar-close-button-show nil
        tab-bar-format '(tab-bar-format-history
                         tab-bar-format-tabs
                         tab-bar-separator)
        tab-bar-show 1
        tab-bar-separator ""
        tab-bar-new-tab-to 'rightmost
        tab-bar-tab-name-format-functions '(tab-bar-tab-name-format-hints
                                            tab-bar-tab-name-format-close-button
                                            +tab-bar/tab-name-format
                                            tab-bar-tab-name-format-face
                                            ;; formatting the face on an XPM causes issues so we do it after
                                            +tab-bar/tab-name-format-icons))


  :config
  (defalias #'+tabs/display #'toggle-tab-bar-mode-from-frame)
  (defalias #'+tabs/new #'tab-bar-new-tab)
  (defalias #'+tabs/close #'tab-bar-close-tab)
  (defalias #'+tabs/switch-left #'tab-bar-switch-to-prev-tab)
  (defalias #'+tabs/switch-right #'tab-bar-switch-to-next-tab)
  (tab-bar-mode t))
