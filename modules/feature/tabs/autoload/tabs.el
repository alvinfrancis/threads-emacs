;;; feature/tabs/autoload/workspaces.el -*- lexical-binding: t; -*-

(defalias #'+tabs/display #'elscreen-toggle-display-tab)

;;;###autoload
(defun +tabs/new ()
  (interactive)
  (elscreen-create))

;;;###autoload
(defun +tabs/close ()
  (interactive)
  (elscreen-kill))
