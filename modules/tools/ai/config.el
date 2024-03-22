;;; tools/ai/config.el -*- lexical-binding: t; -*-

(def-package! gptel
  :config (setq gptel-default-mode 'markdown-mode))

(def-package! gptel-curl
  :commands (gptel-curl-get-response))

(def-package! gptel-transient
  :commands (gptel-menu))
