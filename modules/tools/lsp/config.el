;;; tools/lsp/config.el -*- lexical-binding: t; -*-

(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold nil)
(defvar +lsp--optimization-init-p nil)

(define-minor-mode +lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  (if (not +lsp-optimization-mode)
      (setq-default read-process-output-max +lsp--default-read-process-output-max
                    gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                    +lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless +lsp--optimization-init-p
      (setq +lsp--default-read-process-output-max
            ;; DEPRECATED Remove check when 26 support is dropped
            (if (boundp 'read-process-output-max)
                (default-value 'read-process-output-max))
            +lsp--default-gcmh-high-cons-threshold
            (default-value 'gcmh-high-cons-threshold))
      ;; `read-process-output-max' is only available on recent development
      ;; builds of Emacs 27 and above.
      (setq-default read-process-output-max (* 1024 1024))
      ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
      ;;        native JSON library, so we up the GC threshold to stave off
      ;;        GC-induced slowdowns/freezes. Doom uses `gcmh' to enforce its
      ;;        GC strategy, so we modify its variables rather than
      ;;        `gc-cons-threshold' directly.
      (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq +lsp--optimization-init-p t))))

(defvar +lsp-company-backends 'company-capf
  "The backends to prepend to `company-backends' in `lsp-mode' buffers.
Can be a list of backends; accepts any value `company-backends' accepts.")


(def-package! lsp-mode
  :commands lsp-install-server
  :init
  ;; Don't touch ~/.emacs.d, which could be purged without warning
  (setq lsp-session-file (concat doom-etc-dir "lsp-session")
        lsp-server-install-dir (concat doom-etc-dir "lsp/"))
  ;; Don't auto-kill LSP server after last workspace buffer is killed, because I
  ;; will do it for you, after `+lsp-defer-shutdown' seconds.
  (setq lsp-keep-workspace-alive nil)

  ;; NOTE I tweak LSP's defaults in order to make its more expensive or imposing
  ;;      features opt-in. Some servers implement these poorly and, in most
  ;;      cases, it's safer to rely on Emacs' native mechanisms (eldoc vs
  ;;      lsp-ui-doc, open in popup vs sideline, etc).

  ;; Disable features that have great potential to be slow.
  (setq lsp-enable-folding nil
        lsp-enable-text-document-color nil)
  ;; Reduce unexpected modifications to code
  (setq lsp-enable-on-type-formatting nil)

  ;; Let doom bind the lsp keymap.
  (when (featurep! :config default +bindings)
    (setq lsp-keymap-prefix nil))

  :config
  (setq lsp-intelephense-storage-path (concat doom-cache-dir "lsp-intelephense/")
        lsp-clients-emmy-lua-jar-path (concat lsp-server-install-dir "EmmyLua-LS-all.jar")
        lsp-xml-jar-file              (concat lsp-server-install-dir "org.eclipse.lsp4xml-0.3.0-uber.jar")
        lsp-groovy-server-file        (concat lsp-server-install-dir "groovy-language-server-all.jar"))

  (set! :popup "^\\*lsp-help" :size 0.35 :quit t :select t)
  ;; (set-lookup-handlers! 'lsp-mode :async t
  ;;   :documentation #'lsp-describe-thing-at-point
  ;;   :definition #'lsp-find-definition
  ;;   :implementations #'lsp-find-implementation
  ;;   :type-definition #'lsp-find-type-definition
  ;;   :references #'lsp-find-references)

  (add-hook! 'lsp-mode-hook
    (defun +lsp-display-guessed-project-root-h ()
      "Log what LSP thinks is the root of the current project."
      ;; Makes it easier to detect root resolution issues.
      (when-let (path (buffer-file-name (buffer-base-buffer)))
        (if-let (root (lsp--calculate-root (lsp-session) path))
            (lsp--info "Guessed project root is %s" (abbreviate-file-name root))
          (lsp--info "Could not guess project root."))))
    #'+lsp-optimization-mode)

  (add-hook! 'lsp-completion-mode-hook
    (defun +lsp-init-company-backends-h ()
      (when lsp-completion-mode
        (set (make-local-variable 'company-backends)
             (cons +lsp-company-backends
                   (remove +lsp-company-backends
                           (remq 'company-capf company-backends))))))))


(def-package! lsp-ui
  :defer t
  :config
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t
        ;; lsp-ui-doc is redundant with and more invasive than
        ;; `+lookup/documentation'
        lsp-ui-doc-enable nil
        lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
        lsp-ui-doc-position 'at-point
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil)

  (map! :map lsp-ui-peek-mode-map
        "j"   #'lsp-ui-peek--select-next
        "k"   #'lsp-ui-peek--select-prev
        "C-k" #'lsp-ui-peek--select-prev-file
        "C-j" #'lsp-ui-peek--select-next-file)

  ;; (when (featurep! +peek)
  ;;   (set-lookup-handlers! 'lsp-ui-mode :async t
  ;;     :definition 'lsp-ui-peek-find-definitions
  ;;     :implementations 'lsp-ui-peek-find-implementation
  ;;     :references 'lsp-ui-peek-find-references))
  )


(def-package! helm-lsp
  :when (featurep! :completion helm)
  :commands helm-lsp-workspace-symbol helm-lsp-global-workspace-symbol)
