;;; lang/org/contrib/dragndrop.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dragndrop)

(use-package! org-download
  :commands (org-download-dnd org-download-dnd-base64)
  :init
  ;; HACK We add these manually so that org-download is truly lazy-loaded
  (nconcq! dnd-protocol-alist
           '(("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . +org-dragndrop-download-dnd)
             ("^data:" . org-download-dnd-base64)))
  (advice-add #'org-download-enable :override #'ignore)
  :config
  (setq org-download-image-dir org-attach-directory
        org-download-heading-lvl nil
        org-download-timestamp "_%Y%m%d_%H%M%S"
        org-download-screenshot-method
        (cond (IS-MAC "screencapture -i %s")
              (IS-LINUX
               (cond ((executable-find "maim")  "maim -s %s")
                     ((executable-find "scrot") "scrot -s %s")))))

  ;; Handle non-image files a little differently. Images should be inserted
  ;; as-is, as image previews. Other files, like pdfs or zips, should be linked
  ;; to, with an icon indicating the type of file.
  (defadvice! +org--dragndrop-insert-link-a (_link filename)
    "Produces and inserts a link to FILENAME into the document.

If FILENAME is an image, produce an attach:%s path, otherwise use file:%s (with
an file icon produced by `+org-attach--icon')."
    :override #'org-download-insert-link
    (if (looking-back "^[ \t]+" (line-beginning-position))
        (delete-region (match-beginning 0) (match-end 0))
      (newline))
    (cond ((image-type-from-file-name filename)
           (insert
            (concat (if (= org-download-image-html-width 0) ""
                      (format "#+attr_html: :width %dpx\n" org-download-image-html-width))
                    (if (= org-download-image-latex-width 0) ""
                      (format "#+attr_latex: :width %dcm\n" org-download-image-latex-width))
                    (cond ((file-in-directory-p filename org-attach-directory)
                           (format "[[attach:%s]]" (file-relative-name filename org-attach-directory)))
                          ((file-in-directory-p filename org-directory)
                           (format org-download-link-format (file-relative-name filename org-directory)))
                          ((format org-download-link-format filename)))))
           (org-display-inline-images))
          ((insert
            (format "%s [[./%s][%s]] "
                    (+org-attach--icon filename)
                    (file-relative-name filename (file-name-directory buffer-file-name))
                    (file-name-nondirectory (directory-file-name filename)))))))

  (advice-add #'org-download--dir-2 :override #'ignore)
  (defadvice! +org--dragndrop-download-fullname-a (path)
    "Write PATH relative to current file."
    :filter-return #'org-download--fullname
    (let ((dir (or (if buffer-file-name (file-name-directory buffer-file-name))
                   default-directory)))
      (if (file-in-directory-p dir org-directory)
          (file-relative-name path dir)
        path))))
