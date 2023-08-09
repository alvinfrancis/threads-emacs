;;; tools/impatient-mode/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +impatient-mode/toggle ()
  "TODO"
  (interactive)
  (require 'simple-httpd)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (if impatient-mode
      (add-hook 'kill-buffer-hook '+impatient-mode--cleanup-impatient-mode)
    (+impatient-mode--cleanup-impatient-mode)))

(defun +impatient-mode--cleanup-impatient-mode ()
  (unless (cl-loop for buf in (doom-buffer-list)
                   if (with-current-buffer buf
                        (or impatient-mode (default-value 'impatient-mode)))
                   return t)
    (httpd-stop)
    (cl-loop for buf in (doom-buffer-list)
             if (with-current-buffer buf
                  (or impatient-mode (default-value 'impatient-mode)))
             do
             (with-current-buffer buf
               (impatient-mode -1)))
    (remove-hook 'kill-buffer-hook '+impatient-mode--cleanup-impatient-mode)))
