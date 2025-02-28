;; -*- no-byte-compile: t; -*-
;;; tools/ai/packages.el

(package! gptel)

(package! copilot
  :recipe (:fetcher github :repo "copilot-emacs/copilot.el" :file ("*.el")))

;; Add to ~/.authinfo the following line
;; machine api.openai.com login apikey password TOKEN
