;; -*- no-byte-compile: t; -*-
;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(threads!
 :feature
 eval              ; run code, run (also, repls)
 evil              ; come to the dark side, we have cookies
 ; services          ; TODO managing external services & code builders
 ; spellcheck        ; tasing you for misspelling mispelling
 syntax-checker    ; tasing you for every semicolon you forget
 version-control   ; remember, remember that commit in November
 tabs

 :completion
 company           ; the ultimate code completion backend
 (helm +childframe)  ; the *other* search engine for love and life

 :ui
 doom              ; what makes DOOM look the way it does
 doom-modeline     ; a snazzy Atom-inspired mode-line
 ;; modeline
 doom-quit         ; DOOM quit-message prompts when you quit Emacs
 hl-todo           ; highlight TODO/FIXME/NOTE tags
 ; unicode           ; extended unicode support for various languages
 (window-select +ace-window)  ; visually switch windows

 :tools
 dired             ; making dired pretty [functional]
 electric-indent   ; smarter, keyword-based electric-indent
 eshell            ; a consistent, cross-platform shell (WIP)
 lsp
 debugger
 ; gist              ; interacting with github gists
 ; imenu             ; an imenu sidebar and searchable code index
 ; impatient-mode    ; show off code over HTTP
 ; macos             ; MacOS-specific commands
 ; make              ; run make tasks from Emacs
 ; neotree           ; a project drawer, like NERDTree for vim
 ; password-store    ; password manager for nerds
 ; rotate-text       ; cycle region at point between text candidates
 ; term              ; terminals in Emacs
 ; tmux              ; an API for interacting with tmux
 ; upload            ; map local to remote projects via ssh/ftp

 :lang
 clojure           ; java with a lisp
 data              ; config/data formats
 emacs-lisp        ; drown in parentheses
 go                ; the hipster dialect TODO: remove/rely on lsp
 (java +meghanada) ; the poster child for carpal tunnel syndrome
 javascript        ; all(hope(abandon(ye(who(enter(here))))))
 typescript
 lisp
 markdown          ; writing docs for people to ignore
 (org              ; organize your plain life in plain text
  +babel           ; running code in org
  )
 plantuml
 rest              ; Emacs as a REST client
 scala             ; java, but good
 ;; sh                ; she sells (ba|z)sh shells on the C xor
 web               ; the tubes

 ;; Applications are complex and opinionated modules that transform Emacs
 ;; toward a specific purpose. They may have additional dependencies and
 ;; should be loaded late.
 ;; TODO: drop app modules
 :app
 ;email             ; emacs as an email client
 ;irc               ; how neckbeards socialize
 ;rss               ; emacs as an RSS reader
 ;twitter           ; twitter client https://twitter.com/vnought
 ;write             ; emacs as a word processor (latex + org + markdown)

 ;; Private modules are where you place your personal configuration files.
 ;; By default, they are not tracked. There is one module included here,
 ;; the defaults module. It contains a Spacemacs-inspired keybinding
 ;; scheme and additional ex commands for evil-mode. Use it as a reference
 ;; for your own.
 :private default)

