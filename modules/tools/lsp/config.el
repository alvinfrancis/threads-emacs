;;; tools/lsp/config.el -*- lexical-binding: t; -*-

(def-package! lsp-mode
  :commands (lsp lsp-deferred lsp-install-server)
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
        lsp-enable-indentation nil
        lsp-enable-text-document-color nil)

  ;; Reduce unexpected modifications to code
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-keymap-prefix nil)

  :config
  ;; TODO: move to config bindings?
  (map! :map lsp-mode-map
        :localleader
        :nv "l" lsp-command-map)

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
          (lsp--info "Could not guess project root."))))))

(def-package! lsp-lens
  :commands (lsp-lens-mode))

(def-package! lsp-diagnostics
  :commands (lsp-diagnostics-mode))

(def-package! lsp-headerline
  :commands (lsp-headerline-breadcrumb-mode)
  :init
  (setq lsp-headerline-breadcrumb-enable nil))

(def-package! lsp-modeline
  :commands (lsp-modeline-code-actions-mode lsp-modeline-diagnostics-mode)
  :init
  (setq lsp-modeline-workspace-status-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil))


(def-package! lsp-ui
  :commands (lsp-ui-mode)
  :config
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t
        ;; lsp-ui-doc is redundant with and more invasive than
        ;; `+lookup/documentation'
        lsp-ui-doc-enable nil
        lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t ; in lieu of flycheck
        )

  (map! :map lsp-ui-peek-mode-map
        "j"   #'lsp-ui-peek--select-next
        "k"   #'lsp-ui-peek--select-prev
        "C-k" #'lsp-ui-peek--select-prev-file
        "C-j" #'lsp-ui-peek--select-next-file))


(def-package! lsp-javascript
  :after lsp-mode
  :config
  ;; NOTE: Overriding tls client configuration here
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                            `(,(lsp-package-path 'typescript-language-server)
                                                              ;; NOTE: The following lines were causing EPIPE errors on startup
                                                              ;; "--tsserver-path"
                                                              ;; ,(lsp-package-path 'typescript)
                                                              ,@lsp-clients-typescript-server-args)))
                    :activation-fn 'lsp-typescript-javascript-tsx-jsx-activate-p
                    :priority -2
                    :completion-in-comments? t
                    :initialization-options (lambda ()
                                              (list :plugins lsp-clients-typescript-plugins
                                                    :logVerbosity lsp-clients-typescript-log-verbosity
                                                    :tsServerPath (lsp-package-path 'typescript)))
                    :ignore-messages '("readFile .*? requested by TypeScript but content not available")
                    :server-id 'ts-ls
                    :request-handlers (ht ("_typescript.rename" #'lsp-javascript--rename))
                    :download-server-fn (lambda (_client callback error-callback _update?)
                                          (lsp-package-ensure
                                           'typescript
                                           (-partial #'lsp-package-ensure
                                                     'typescript-language-server
                                                     callback
                                                     error-callback)
                                           error-callback)))))


(def-package! helm-lsp
  :when (featurep! :completion helm)
  :commands helm-lsp-workspace-symbol helm-lsp-global-workspace-symbol)
