;;; tools/ai/config.el -*- lexical-binding: t; -*-

(def-package! gptel
  :config (setq gptel-default-mode 'markdown-mode))

(def-package! gptel-curl
  :commands (gptel-curl-get-response))

(def-package! gptel-transient
  :commands (gptel-menu))

(def-package! copilot
 :hook (prog-mode . copilot-mode)
 :bind (:map copilot-completion-map
             ("<tab>" . 'copilot-accept-completion)
             ("TAB" . 'copilot-accept-completion)
             ("C-TAB" . 'copilot-accept-completion-by-word)
             ("C-<tab>" . 'copilot-accept-completion-by-word)))
