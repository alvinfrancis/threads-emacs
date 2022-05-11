;;; lang/web/autoload/css.el -*- lexical-binding: t; -*-

;;;###autoload
;; TODO (defun +css/scss-build ())

;;;###autoload
;; TODO (defun +css/sass-build ())

;;;###autoload
(defun +css/toggle-px-rem ()
  "Toggles thing at point between rems and pixels."
  (interactive)
  (let ((text (thing-at-point 'sexp))
        (location (bounds-of-thing-at-point 'sexp)))
    (cond
     ((s-ends-with? "rem" text)
      (delete-region (car location) (cdr location))
      (insert (format "%dpx" (* 16 (string-to-number text)))))
     ((s-ends-with? "px" text)
      (delete-region (car location) (cdr location))
      (insert (format "%.10frem" (/ (string-to-number text) 16.0)))))))

;;;###autoload
(defun +css/toggle-inline-or-block ()
  "Toggles between a bracketed block and inline block."
  (interactive)
  ;; TODO Remove evil dependency
  (save-excursion
    (cl-destructuring-bind (beg end)
        (or (ignore-errors (evil-a-curly))
            (user-error "No block found"))
      (if (= (line-number-at-pos beg) (line-number-at-pos end))
          (save-excursion
            (goto-char (1+ beg)) (insert "\n")
            (unless (string-match ";[\s\t]*}$" (buffer-substring-no-properties beg (1+ end)))
              (goto-char end) (insert "\n"))
            (while (re-search-forward ";[\s\t]*" (1+ end) t)
              (replace-match ";\n" t t))
            (setq end (cadr (evil-a-curly)))
            (evil-indent beg end)
            (delete-trailing-whitespace beg end))
        (goto-char beg)
        (evil-join beg end)
        (goto-char (1+ beg))
        (just-one-space)
        (goto-char (cadr (evil-inner-curly)))
        (just-one-space)))))
