;; -*- no-byte-compile: t; -*-
;;; core/test/autoload-package.el

(defun -pkg (name version &optional reqs)
  (package-desc-create :name name :version version :reqs reqs :dir (expand-file-name (symbol-name name) package-user-dir)))

(defmacro with-packages!! (packages package-descs &rest body)
`(let* ((threads-packages-dir ,(expand-file-name "packages/" (file-name-directory load-file-name)))
        (package-user-dir ,(expand-file-name "elpa" threads-packages-dir))
        (quelpa-dir ,(expand-file-name "quelpa" threads-packages-dir)))
   ;; (make-directory threads-packages-dir t)
   (let ((threads-packages ,packages)
         (package-alist ,package-descs)
         threads-core-packages)
     (cl-letf (((symbol-function 'threads-initialize-packages) (lambda (&rest _))))
       ,@body))
   ;; (delete-directory threads-packages-dir t)
   ))


;;
;; Tests
;;

(def-test! backend-detection
  (let ((package-alist `((doom-dummy ,(-pkg 'doom-dummy '(20160405 1234)))))
        (quelpa-cache '((doom-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist")))
        (quelpa-initialized-p t))
    (should (eq (threads-package-backend 'doom-dummy) 'elpa))
    (should (eq (threads-package-backend 'doom-quelpa-dummy) 'quelpa))
    (should (eq (threads-package-backend 'org) 'emacs))))

(def-test! elpa-outdated-detection
  (let* ((threads--last-refresh (current-time))
         (package-alist
          `((doom-dummy ,(-pkg 'doom-dummy '(20160405 1234)))))
         (package-archive-contents
          `((doom-dummy ,(-pkg 'doom-dummy '(20170405 1234))))))
    (cl-letf (((symbol-function 'package-refresh-contents) (lambda (&rest _))))
      (should (equal (threads-package-outdated-p 'doom-dummy)
                     '(doom-dummy (20160405 1234) (20170405 1234)))))))

;; TODO quelpa-outdated-detection

(def-test! get-packages
  (let ((quelpa-initialized-p t))
    (with-packages!!
     '((doom-dummy))
     '((doom-dummy          nil)
       (doom-dummy-unwanted nil)
       (doom-dummy-dep      nil))
     (should (equal (threads-get-packages) '((doom-dummy)))))))

(def-test! orphaned-packages
  "Test `threads-get-orphaned-packages', which gets a list of packages that are
no longer enabled or depended on."
  (with-packages!!
   '((doom-dummy))
   `((doom-dummy          ,(-pkg 'doom-dummy '(20160405 1234) '((doom-dummy-dep (1 0)))))
     (doom-dummy-unwanted ,(-pkg 'doom-dummy-unwanted '(20160601 1234)))
     (doom-dummy-dep      ,(-pkg 'doom-dummy-dep '(20160301 1234))))
   (should (equal (threads-get-orphaned-packages) '(doom-dummy-unwanted)))))

(def-test! missing-packages
  "Test `threads-get-missing-packages, which gets a list of enabled packages that
aren't installed."
  (with-packages!!
   '((doom-dummy) (doom-dummy-installed))
   `((doom-dummy-installed ,(-pkg 'doom-dummy-installed '(20160405 1234))))
   (should (equal (threads-get-missing-packages) '((doom-dummy))))))
