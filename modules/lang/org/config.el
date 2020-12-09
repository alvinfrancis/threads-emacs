;;; lang/org/config.el -*- lexical-binding: t; -*-

(defvar +org-dir (expand-file-name "~/Documents/org/")
  "The directory where org files are kept.")

(if (featurep! +babel) (load! +babel))

;; (after! org
;;   ;; Occasionally, Emacs encounters an error loading the built-in org, aborting
;;   ;; the load. This results in a broken, partially loaded state. This require
;;   ;; tries to set it straight.
;;   (require 'org)

;;   (defvaralias 'org-directory '+org-dir)

;;   (org-crypt-use-before-save-magic)
;;   (+org-init-ui)
;;   (+org-init-keybinds))

(def-package! org
  :init
  (setq org-fontify-quote-and-verse-blocks t
        org-src-fontify-natively t
        org-tags-exclude-from-inheritance (quote ("crypt"))
        org-crypt-key "alvin.francis.dumalus@gmail.com"
        org-hide-emphasis-markers t
        org-adapt-indentation nil
        org-imenu-depth 3)

  (add-hook! org-mode
  #'(visual-line-mode           ; line wrapping

     +org|smartparens-compatibility-config
     +org|unfold-to-2nd-level-or-point
     +org|show-paren-mode-compatibility)))


;;
;; Config hooks
;;

(defun +org|unfold-to-2nd-level-or-point ()
  "My version of the 'overview' #+STARTUP option: expand first-level headings.
Expands the first level, but no further. If point was left somewhere deeper,
unfold to point on startup."
  (unless org-agenda-inhibit-startup
    (when (eq org-startup-folded t)
      (outline-hide-sublevels 2))
    (when (outline-invisible-p)
      (ignore-errors
        (save-excursion
          (outline-previous-visible-heading 1)
          (org-show-subtree))))))

(defun +org|smartparens-compatibility-config ()
  "Instruct `smartparens' not to impose itself in org-mode."
  (defun +org-sp-point-in-checkbox-p (_id action _context)
    (when (eq action 'insert)
      (sp--looking-at-p "\\s-*]")))

  ;; make delimiter auto-closing a little more conservative
  (after! smartparens
    (sp-with-modes 'org-mode
      (sp-local-pair "*" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-at-bol-p))
      (sp-local-pair "_" nil :unless '(sp-point-after-word-p sp-point-before-word-p))
      (sp-local-pair "/" nil :unless '(sp-point-after-word-p sp-point-before-word-p +org-sp-point-in-checkbox-p))
      (sp-local-pair "~" nil :unless '(sp-point-after-word-p sp-point-before-word-p))
      (sp-local-pair "=" nil :unless '(sp-point-after-word-p sp-point-before-word-p)))))

(defun +org|show-paren-mode-compatibility ()
  "`show-paren-mode' causes flickering with indentation margins made by
`org-indent-mode', so we simply turn off show-paren-mode altogether."
  (set (make-local-variable 'show-paren-mode) nil))


(defun +org-init-keybinds ()
  "Sets up org-mode and evil keybindings. Tries to fix the idiosyncrasies
between the two."
  (map! :map org-mode-map
        "RET" #'org-return-indent
        "C-c C-S-l" #'+org/remove-link
        :n "C-c C-i" #'org-toggle-inline-images

        :n  "RET" #'+org/dwim-at-point

        ;; Navigate table cells (from insert-mode)
        :i  "C-l"   #'+org/table-next-field
        :i  "C-h"   #'+org/table-previous-field
        :i  "C-k"   #'+org/table-previous-row
        :i  "C-j"   #'+org/table-next-row
        ;; Expand tables (or shiftmeta move)
        :ni "C-S-l" #'+org/table-append-field-or-shift-right
        :ni "C-S-h" #'+org/table-prepend-field-or-shift-left
        :ni "C-S-k" #'org-metaup
        :ni "C-S-j" #'org-metadown

        :n  [tab]     #'+org/toggle-fold
        :i  [tab]     #'org-cycle
        :i  [backtab] #'+org/dedent-or-prev-field

        :ni [M-return]   (λ! (+org/insert-item 'below))
        :ni [S-M-return] (λ! (+org/insert-item 'above))

        :m  "]]"  (λ! (org-forward-heading-same-level nil) (org-beginning-of-line))
        :m  "[["  (λ! (org-backward-heading-same-level nil) (org-beginning-of-line))
        :m  "]l"  #'org-next-link
        :m  "[l"  #'org-previous-link
        :m  "$"   #'org-end-of-line
        :m  "^"   #'org-beginning-of-line
        :n  "gQ"  #'org-fill-paragraph
        :n  "<"   #'org-metaleft
        :n  ">"   #'org-metaright
        :v  "<"   (λ! (org-metaleft)  (evil-visual-restore))
        :v  ">"   (λ! (org-metaright) (evil-visual-restore))

        ;; Fix code-folding keybindings
        :n  "za"  #'+org/toggle-fold
        :n  "zA"  #'org-shifttab
        :n  "zc"  #'outline-hide-subtree
        :n  "zC"  (λ! (outline-hide-sublevels 1))
        :n  "zd"  (lambda (&optional arg) (interactive "p") (outline-hide-sublevels (or arg 3)))
        :n  "zm"  (λ! (outline-hide-sublevels 1))
        :n  "zo"  #'outline-show-subtree
        :n  "zO"  #'outline-show-all
        :n  "zr"  #'outline-show-all

        (:after org-agenda
          (:map org-agenda-mode-map
            :e "<escape>" #'org-agenda-Quit
            :e "m"   #'org-agenda-month-view
            :e "C-j" #'org-agenda-next-item
            :e "C-k" #'org-agenda-previous-item
            :e "C-n" #'org-agenda-next-item
            :e "C-p" #'org-agenda-previous-item))))
