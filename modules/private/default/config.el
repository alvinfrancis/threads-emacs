;;; private/default/config.el -*- lexical-binding: t; -*-

(load! +bindings)


;;
;; Plugins
;;


;;
;; Config
;;

(setq doom-font (font-spec :family "Monoid" :size 12)
      doom-variable-pitch-font (font-spec :family "Monoid")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Monoid" :size 19)
      doom-line-numbers-style 'relative
      doom-theme 'doom-gruvbox

      scroll-margin 100          ; TODO: look into a better centering mechanism
      scroll-conservatively 100
      +org-dir "~/Documents/org")

(after! helm-files
  (setq helm-ff-transformer-show-only-basename nil))

(after! epa
  (setq epa-file-encrypt-to (or epa-file-encrypt-to user-mail-address)
        ;; With GPG 2.1, this forces gpg-agent to use the Emacs minibuffer to
        ;; prompt for the key passphrase.
        epa-pinentry-mode 'loopback))

(add-hook! 'before-save-hook #'whitespace-cleanup)

(defun doom*hide-vertical-border (&rest _)
  "Hide vertical border."
  (set-face-attribute 'vertical-border nil :foreground
                      (face-attribute 'default :background)))
(advice-add #'load-theme :after #'doom*hide-vertical-border)


(when (featurep 'evil)
  (load! +evil-commands)

  ;; Makes ; and , the universal repeat-keys in evil-mode
  (defmacro do-repeat! (command next-func prev-func)
    "Repeat motions with ;/,"
    (let ((fn-sym (intern (format "+evil*repeat-%s" command))))
      `(progn
         (defun ,fn-sym (&rest _)
           (define-key evil-motion-state-map (kbd ";") ',next-func)
           (define-key evil-motion-state-map (kbd ",") ',prev-func))
         (advice-add #',command :before #',fn-sym))))

  ;; n/N
  (do-repeat! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)


  ;; */#
  (after! evil-visualstar
    (do-repeat! evil-visualstar/begin-search-forward
                evil-ex-search-next evil-ex-search-previous)
    (do-repeat! evil-visualstar/begin-search-backward
                evil-ex-search-previous evil-ex-search-next)))
