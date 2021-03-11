;;; completion/helm/config.el -*- lexical-binding: t; -*-

;; Warning: since I don't use helm, this may be out of date.

(defvar +helm-global-prompt "››› "
  "The helm text prompt prefix string is globally replaced with this string.")

(defvar +helm-posframe-handler #'+helm-poshandler-frame-center-near-bottom-fn
  "The function that determines the location of the childframe. It should return
a cons cell representing the X and Y coordinates. See
`posframe-poshandler-frame-center' as a reference.")

(defvar +helm-posframe-text-scale 1
  "The text-scale to use in the helm childframe. Set to nil for no scaling. Can
be negative.")

(defvar +helm-posframe-parameters
  '((internal-border-width . 8)
    (width . 0.5)
    (height . 0.35)
    (min-width . 80)
    (min-height . 16))
  "TODO")

(defun +helm-poshandler-frame-center-near-bottom-fn (info)
  "Display the child frame in the center of the frame, slightly closer to the
bottom, which is easier on the eyes on big displays."
  (let ((parent-frame (plist-get info :parent-frame))
        (pos (posframe-poshandler-frame-center info)))
    (cons (car pos)
          (truncate (/ (frame-pixel-height parent-frame)
                       2)))))

(defvar +helm--posframe-buffer nil)
(defun +helm-posframe-display-fn (buffer &optional _resume)
  "TODO"
  (setq helm--buffer-in-new-frame-p t)
  (let ((solaire-p (bound-and-true-p solaire-mode))
        (params (copy-sequence +helm-posframe-parameters)))
    (let-alist params
      (require 'posframe)
      (posframe-show
       (setq +helm--posframe-buffer buffer)
       :position (point)
       :poshandler +helm-posframe-handler
       :respect-header-line helm-echo-input-in-header-line
       :width
       (max (cl-typecase .width
              (integer .width)
              (float (truncate (* (frame-width) .width)))
              (function (funcall .width))
              (t 0))
            .min-width)
       :height
       (max (cl-typecase .height
              (integer .height)
              (float (truncate (* (frame-height) .height)))
              (function (funcall .height))
              (t 0))
            .min-height)
       :override-parameters
       (dolist (p '(width height min-width min-height) params)
         (setq params (delq (assq p params) params)))))
    ;;
    (unless (or (null +helm-posframe-text-scale)
                (= +helm-posframe-text-scale 0))
      (with-current-buffer buffer
        (when (and (featurep 'solaire-mode)
                   (not solaire-p))
          (solaire-mode +1))
        (text-scale-set +helm-posframe-text-scale)))))

;;
;; Packages
;;

(def-package! helm
  :init
  (setq helm-quick-update t
        ;; Speedier without fuzzy matching
        helm-mode-fuzzy-match nil
        helm-buffers-fuzzy-matching nil
        helm-apropos-fuzzy-match nil
        helm-M-x-fuzzy-match nil
        helm-recentf-fuzzy-match nil
        helm-projectile-fuzzy-match nil
        helm-find-files-doc-header nil
        ;; Don't override evil-ex's completion
        helm-mode-handle-completion-in-region nil
        helm-candidate-number-limit 50)

  (when (featurep! +childframe)
    ;; If this is set to 'iconify-top-level then Emacs will be minimized upon
    ;; helm completion.
    (setq iconify-child-frame 'make-invisible)
    ;; (setq helm-display-function #'+helm-posframe-display-fn)
    )

  :config
  (load "helm-autoloads" nil t)
  (add-hook 'doom-init-hook #'helm-mode)

  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  (require 'helm-projectile)
  (set-keymap-parent helm-projectile-find-file-map helm-map)

  ;; helm is too heavy for find-file-at-point
  (after! helm-mode
    (add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil)))

  (set! :popup "\\` ?\\*[hH]elm.*?\\*\\'" :size 14 :regexp t)
  (setq projectile-completion-system 'helm)

  ;;; Helm hacks
  (defun +helm*replace-prompt (plist)
    "Globally replace helm prompts with `+helm-global-prompt'."
    (if (keywordp (car plist))
        (plist-put plist :prompt +helm-global-prompt)
      (setf (nth 2 plist) +helm-global-prompt)
      plist))
  (advice-add #'helm :filter-args #'+helm*replace-prompt)

  (defun +helm*hide-header (&rest _)
    "Hide header-line & mode-line in helm windows."
    (setq mode-line-format nil))
  (advice-add #'helm-display-mode-line :override #'+helm*hide-header)

  (map! :map global-map
        [remap apropos]                   #'helm-apropos
        [remap find-file]                 #'helm-find-files
        [remap recentf-open-files]        #'helm-recentf
        [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer
        [remap projectile-recentf]        #'helm-projectile-recentf
        [remap projectile-find-file]      #'helm-projectile-find-file
        [remap imenu]                     #'helm-semantic-or-imenu
        [remap bookmark-jump]             #'helm-bookmarks
        [remap noop-show-kill-ring]       #'helm-show-kill-ring
        [remap projectile-switch-project] #'helm-projectile-switch-project
        [remap projectile-find-file]      #'helm-projectile-find-file
        [remap imenu-anywhere]            #'helm-imenu-anywhere
        [remap execute-extended-command]  #'helm-M-x
        [remap switch-to-buffer]          #'helm-buffers-list))


(def-package! helm-locate
  :defer t
  :init (defvar helm-generic-files-map (make-sparse-keymap))
  :config (set-keymap-parent helm-generic-files-map helm-map))


(def-package! helm-bookmark
  :commands helm-bookmark
  :config (setq-default helm-bookmark-show-location t))


(def-package! helm-files
  :defer t
  :config
  (setq helm-boring-file-regexp-list
        (append (list "\\.projects$" "\\.DS_Store$")
                helm-boring-file-regexp-list)))


(def-package! helm-ag
  :defer t
  :config
  (map! :map helm-ag-edit-map
        [remap doom/kill-this-buffer] #'helm-ag--edit-abort
        [remap quit-window]           #'helm-ag--edit-abort))


(def-package! helm-css-scss ; https://github.com/ShingoFukuyama/helm-css-scss
  :commands (helm-css-scss
             helm-css-scss-multi
             helm-css-scss-insert-close-comment)
  :config
  (setq helm-css-scss-split-direction #'split-window-vertically
        helm-css-scss-split-with-multiple-windows t))


(def-package! helm-swoop ; https://github.com/ShingoFukuyama/helm-swoop
  :commands (helm-swoop helm-multi-swoop helm-multi-swoop-all)
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-candidate-number-limit 200
        helm-swoop-speed-or-color t
        helm-swoop-pre-input-function (lambda () "")))


(def-package! helm-describe-modes :commands helm-describe-modes)

(def-package! swiper :commands (swiper swiper-all))

(defun +helm--xref-format-candidate-projectile-path (file line summary)
  (let ((path (if (projectile-project-p)
                  (replace-regexp-in-string
                   (regexp-quote (projectile-project-root))
                   ""
                   file)
                file)))
    (concat
     (propertize path
                 'font-lock-face 'helm-xref-file-name)
     (when (string= "integer" (type-of line))
       (concat
        ":"
        (propertize (int-to-string line)
                    'font-lock-face 'helm-xref-line-number)))
     ":"
     summary)))

(def-package! helm-xref
  :after helm
  :init
  (when (featurep 'projectile)
    (setq helm-xref-candidate-formatting-function #'+helm--xref-format-candidate-projectile-path)))
