;;; feature/tabs/autoload/workspaces.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +tabs/new ()
  (interactive)
  (elscreen-create))

;;;###autoload
(defun +tabs/close ()
  (interactive)
  (elscreen-kill))
