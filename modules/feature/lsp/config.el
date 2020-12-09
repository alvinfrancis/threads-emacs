;;; feature/lsp/config.el -*- lexical-binding: t; -*-

(def-package! lsp-mode
  :config (setq lsp-prefer-flymake nil))

(def-package! lsp-ui)

(def-package! dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))
