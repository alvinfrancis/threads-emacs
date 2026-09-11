;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; tools/ai/packages.el

(package! gptel)

(package! copilot
  :recipe (:fetcher github :repo "copilot-emacs/copilot.el" :file ("*.el")))

(package! mcp)

(package! acp)

(package! agent-shell)

(package! agent-shell-attention
  :recipe (:fetcher github :repo "ultronozm/agent-shell-attention.el" :file ("*.el")))

(package! eca)

