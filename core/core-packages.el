;;; core-packages.el --- package management system -*- lexical-binding: t; -*-
;;
;; The three key commands are:
;;
;; + `make install` or `threads//packages-install': Installs packages that are
;;   wanted, but not installed.
;; + `make update` or `threads//packages-update': Updates packages that are
;;   out-of-date.
;; + `make autoremove` or `threads//packages-autoremove': Uninstalls packages that
;;   are no longer needed.
;;
;; This system reads packages.el files located in each activated module (and one
;; in `threads-core-dir'). These contain `package!' blocks that tell DOOM what
;; plugins to install and where from.
;;
;;
;; package.el command alternatives:
;;
;;    + `package-install':          `threads/install-package'
;;    + `package-reinstall':        `threads/reinstall-package'
;;    + `package-delete':           `threads/delete-package'
;;    + `package-update':           `threads/update-package'
;;    + `package-autoremove':       `threads//packages-autoremove'
;;    + `package-refresh-contents': `threads/refresh-packages'
;;
;; See core/autoload/packages.el for more functions.

(defvar threads-init-p nil
  "Non-nil if Threads is done initializing (once `threads-post-init-hook' is done). If
this is nil after Emacs has started something is wrong.")

(defvar threads-init-time nil
  "The time it took, in seconds, for Threads Emacs to initialize.")

(defvar threads-modules ()
  "A hash table of enabled modules. Set by `threads-initialize-modules'.")

(defvar threads-packages ()
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`package!' declaration. Set by `threads-initialize-packages'.")

(defvar threads-core-packages
  '(persistent-soft use-package quelpa async)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar threads-disabled-packages ()
  "A list of packages that should be ignored by `def-package!'.")

(defvar threads-reload-hook nil
  "A list of hooks to run when `threads/reload-load-path' is called.")

(defvar threads--site-load-path load-path
  "The load path of built in Emacs libraries.")

(defvar threads--package-load-path ()
  "The load path of package libraries installed via ELPA and QUELPA.")

(defvar threads--base-load-path
  (append (list threads-core-dir threads-modules-dir)
          threads--site-load-path)
  "A backup of `load-path' before it was altered by `threads-initialize'. Used as a
base by `threads!' and for calculating how many packages exist.")

(defvar threads--refreshed-p nil)

(setq package--init-file-ensured t
      package-user-dir (expand-file-name "elpa" threads-packages-dir)
      package-enable-at-startup nil
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.

      ;; security settings
      gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")

      use-package-verbose threads-debug-mode
      use-package-minimum-reported-time (if threads-debug-mode 0 0.1)

      ;; Don't track MELPA, we'll use package.el for that
      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-self-upgrade-p nil
      quelpa-verbose threads-debug-mode
      quelpa-dir (expand-file-name "quelpa" threads-packages-dir)

      byte-compile-verbose threads-debug-mode
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))


;;
;; Bootstrap function
;;

(defun threads-initialize (&optional force-p)
  "Initialize installed packages (using package.el) and ensure the core packages
are installed.

If you byte-compile core/core.el, this function will be avoided to speed up
startup."
  ;; Called early during initialization; only use native (and cl-lib) functions!
  (when (or force-p (not threads-init-p))
    ;; Speed things up with a `load-path' for only the bare essentials
    (let ((load-path threads--base-load-path))
      ;; Ensure core folders exist, otherwise we get errors
      (dolist (dir (list threads-local-dir threads-etc-dir threads-cache-dir threads-packages-dir))
        (unless (file-directory-p dir)
          (make-directory dir t)))
      ;; Ensure package.el is initialized; we use its state
      (setq package-activated-list nil)
      (condition-case _ (package-initialize t)
        ('error (package-refresh-contents)
                (setq threads--refreshed-p t)
                (package-initialize t)))
      ;; Ensure core packages are installed
      (let ((core-packages (cl-remove-if #'package-installed-p threads-core-packages)))
        (when core-packages
          (message "Installing core packages")
          (unless threads--refreshed-p
            (package-refresh-contents))
          (dolist (package core-packages)
            (let ((inhibit-message t))
              (package-install package))
            (if (package-installed-p package)
                (message "✓ Installed %s" package)
              (error "✕ Couldn't install %s" package)))
          (message "Installing core packages...done")))
      (setq threads-init-p t))))

(defun threads-initialize-load-path (&optional force-p)
  (when (or force-p (not threads--package-load-path))
    ;; We could let `package-initialize' fill `load-path', but it does more than
    ;; that alone (like load autoload files). If you want something prematurely
    ;; optimizated right, ya gotta do it yourself.
    ;;
    ;; Also, in some edge cases involving package initialization during a
    ;; non-interactive session, `package-initialize' fails to fill `load-path'.
    (setq threads--package-load-path (directory-files package-user-dir t "^[^.]" t)
          load-path (append threads--base-load-path threads--package-load-path))))

(defun threads-initialize-autoloads ()
  "Ensures that `threads-autoload-file' exists and is loaded. Otherwise run
`threads//reload-autoloads' to generate it."
  (unless (file-exists-p threads-autoload-file)
    (quiet! (threads//reload-autoloads))))

(defun threads-initialize-packages (&optional force-p load-p)
  "Crawls across your emacs.d to fill `threads-modules' (from init.el) and
`threads-packages' (from packages.el files), if they aren't set already.

If FORCE-P is non-nil, do it even if they are.

This aggressively reloads core autoload files."
  (threads-initialize-load-path force-p)
  (with-temp-buffer ; prevent buffer-local settings from propagating
    (cl-flet
        ((_load
          (file &optional noerror interactive)
          (condition-case-unless-debug ex
              (let ((load-prefer-newer t)
                    (noninteractive (not interactive)))
                (load file noerror :nomessage :nosuffix))
            ('error
             (lwarn 'threads-initialize-packages :warning
                    "%s in %s: %s"
                    (car ex)
                    (file-relative-name file threads-emacs-dir)
                    (error-message-string ex))))))
      (when (or force-p (not threads-modules))
        (setq threads-modules nil
              threads-packages nil)
        (_load (concat threads-core-dir "core.el") nil 'interactive)
        (_load (expand-file-name "init.el" threads-emacs-dir))
        (when load-p
          (mapc #'_load (file-expand-wildcards (expand-file-name "autoload/*.el" threads-core-dir)))
          (_load (expand-file-name "init.el" threads-emacs-dir) nil 'interactive)))
      (when (or force-p (not threads-packages))
        (setq threads-packages nil)
        (_load (expand-file-name "packages.el" threads-core-dir))
        (cl-loop for (module . submodule) in (threads-module-pairs)
                 for path = (threads-module-path module submodule "packages.el")
                 do (_load path 'noerror))))))

(defun threads-initialize-modules (modules)
  "Adds MODULES to `threads-modules'. MODULES must be in mplist format.

  e.g '(:feature evil :lang emacs-lisp javascript java)"
  (unless threads-modules
    (setq threads-modules (make-hash-table :test #'equal
                                        :size (+ 5 (length modules))
                                        :rehash-threshold 1.0)))
  (let (mode)
    (dolist (m modules)
      (cond ((keywordp m) (setq mode m))
            ((not mode)   (error "No namespace specified on `threads!' for %s" m))
            ((listp m)    (threads-module-enable mode (car m) (cdr m)))
            (t            (threads-module-enable mode m))))))

(defun threads-module-path (module submodule &optional file)
  "Get the full path to a module: e.g. :lang emacs-lisp maps to
~/.emacs.d/modules/lang/emacs-lisp/ and will append FILE if non-nil."
  (when (keywordp module)
    (setq module (substring (symbol-name module) 1)))
  (when (symbolp submodule)
    (setq submodule (symbol-name submodule)))
  (expand-file-name (concat module "/" submodule "/" file)
                    threads-modules-dir))

(defun threads-module-from-path (path)
  "Get module cons cell (MODULE . SUBMODULE) for PATH, if possible."
  (when-let* ((path (file-relative-name (file-truename path) (file-truename threads-modules-dir))))
    (let ((segments (split-string path "/")))
      (cons (intern (concat ":" (car segments)))
            (intern (cadr segments))))))

(defun threads-module-paths (&optional append-file)
  "Returns a list of absolute file paths to activated modules, with APPEND-FILE
added, if the file exists."
  (cl-loop for (module . submodule) in (threads-module-pairs)
           for path = (threads-module-path module submodule append-file)
           if (file-exists-p path)
           collect path))

(defun threads-module-get (module submodule)
  "Returns a list of flags provided for MODULE SUBMODULE."
  (gethash (cons module submodule) threads-modules))

(defun threads-module-enabled-p (module submodule)
  "Returns t if MODULE->SUBMODULE is present in `threads-modules'."
  (and (threads-module-get module submodule) t))

(defun threads-module-enable (module submodule &optional flags)
  "Adds MODULE and SUBMODULE to `threads-modules', overwriting it if it exists.

MODULE is a keyword, SUBMODULE is a symbol. e.g. :lang 'emacs-lisp.

Used by `require!' and `depends-on!'."
  (let ((key (cons module submodule)))
    (puthash key
             (or (threads-enlist flags)
                 (gethash key threads-modules)
                 '(t))
             threads-modules)))

(defun threads-module-pairs ()
  "Returns `threads-modules' as a list of (MODULE . SUBMODULE) cons cells. The list
is sorted by order of insertion unless ALL-P is non-nil. If ALL-P is non-nil,
include all modules, enabled or otherwise."
  (unless (hash-table-p threads-modules)
    (error "threads-modules is uninitialized"))
  (cl-loop for key being the hash-keys of threads-modules
           collect key))

(defun threads-packages--display-benchmark ()
  (message "Threads loaded %s packages across %d modules in %.03fs"
           ;; Certainly imprecise, especially where custom additions to
           ;; load-path are concerned, but I don't mind a [small] margin of
           ;; error in the plugin count in exchange for faster startup.
           (length threads--package-load-path)
           (hash-table-size threads-modules)
           (setq threads-init-time (float-time (time-subtract after-init-time before-init-time)))))


;;
;; Macros
;;

(autoload 'use-package "use-package" nil nil 'macro)

(defmacro threads! (&rest modules)
  "Bootstrap Threads Emacs.

MODULES is an malformed plist of modules to load."
  (threads-initialize-modules modules)
  `(let (file-name-handler-alist)
     (setq threads-modules ',threads-modules)
     (unless noninteractive
       (message "Threads initialized")
       ,@(cl-loop for (module . submodule) in (threads-module-pairs)
                  for module-path = (threads-module-path module submodule)
                  collect `(load! init ,module-path t) into inits
                  collect `(load! config ,module-path t) into configs
                  finally return (append inits configs))
       (when (display-graphic-p)
         (require 'server)
         (unless (server-running-p)
           (server-start)))
       (add-hook 'threads-init-hook #'threads-packages--display-benchmark t)
       (message "Threads modules initialized"))))

(defmacro def-package! (name &rest plist)
  "A thin wrapper around `use-package'."
  ;; Ignore package if NAME is in `threads-disabled-packages'
  (when (and (memq name threads-disabled-packages)
             (not (memq :disabled plist)))
    (setq plist `(:disabled t ,@plist)))
  ;; If byte-compiling, ignore this package if it doesn't meet the condition.
  ;; This avoids false-positive load errors.
  (unless (and (bound-and-true-p byte-compile-current-file)
               (or (and (plist-member plist :if)     (not (eval (plist-get plist :if))))
                   (and (plist-member plist :when)   (not (eval (plist-get plist :when))))
                   (and (plist-member plist :unless) (eval (plist-get plist :unless)))))
    `(use-package ,name ,@plist)))

(defmacro def-package-hook! (package when &rest body)
  "Reconfigures a package's `def-package!' block.

Under the hood, this uses use-package's `use-package-inject-hooks'.

PACKAGE is a symbol; the package's name.
WHEN should be one of the following:
  :pre-init :post-init :pre-config :post-config :disable

If WHEN is :disable then BODY is ignored, and Threads will be instructed to ignore
all `def-package!' blocks for PACKAGE.

WARNING: If :pre-init or :pre-config hooks return nil, the original
`def-package!''s :init/:config block (respectively) is overwritten, so remember
to have them return non-nil (or exploit that to overwrite Threads config)."
  (declare (indent defun))
  (cond ((eq when :disable)
         (push package threads-disabled-packages)
         nil)
        ((memq when '(:pre-init :post-init :pre-config :post-config))
         `(progn
            (setq use-package-inject-hooks t)
            (add-hook!
              ',(intern (format "use-package--%s--%s-hook"
                                package
                                (substring (symbol-name when) 1)))
              ,@body)))
        (t
         (error "'%s' isn't a valid hook for def-package-hook!" when))))

(defmacro load! (filesym &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILESYM is either a symbol or string representing the file to load. PATH is
where to look for the file (a string representing a directory path). If omitted,
the lookup is relative to `load-file-name', `byte-compile-current-file' or
`buffer-file-name' (in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (cl-assert (symbolp filesym) t)
  (let ((path (or path
                  (and load-file-name (file-name-directory load-file-name))
                  (and (bound-and-true-p byte-compile-current-file)
                       (file-name-directory byte-compile-current-file))
                  (and buffer-file-name
                       (file-name-directory buffer-file-name))
                  (error "Could not detect path to look for '%s' in" filesym)))
        (filename (symbol-name filesym)))
    (let ((file (expand-file-name (concat filename ".el") path)))
      (if (file-exists-p file)
          `(load ,(file-name-sans-extension file) ,noerror
                 ,(not threads-debug-mode))
        (unless noerror
          (error "Could not load file '%s' from '%s'" file path))))))

(defmacro require! (module submodule &optional flags reload-p)
  "Loads the module specified by MODULE (a property) and SUBMODULE (a symbol).

The module is only loaded once. If RELOAD-P is non-nil, load it again."
  (when (or reload-p (not (threads-module-enabled-p module submodule)))
    (let ((module-path (threads-module-path module submodule)))
      (if (not (file-directory-p module-path))
          (lwarn 'threads-modules :warning "Couldn't find module '%s %s'"
                 module submodule)
        (threads-module-enable module submodule flags)
        `(condition-case-unless-debug ex
             (load! config ,module-path t)
           ('error
            (lwarn 'threads-modules :error
                   "%s in '%s %s' -> %s"
                   (car ex) ,module ',submodule
                   (error-message-string ex))))))))

(defmacro featurep! (module &optional submodule flag)
  "A convenience macro wrapper for `threads-module-enabled-p'. It is evaluated at
compile-time/macro-expansion time."
  (unless submodule
    (let* ((path (or load-file-name byte-compile-current-file))
           (module-pair (threads-module-from-path path)))
      (unless module-pair
        (error "featurep! couldn't detect what module I'm in! (in %s)" path))
      (setq flag module
            module (car module-pair)
            submodule (cdr module-pair))))
  (if flag
      (and (memq flag (threads-module-get module submodule)) t)
    (threads-module-enabled-p module submodule)))


;;
;; Declarative macros
;;

(defmacro package! (name &rest plist)
  "Declares a package and how to install it (if applicable).

This macro is declarative and does not load nor install packages. It is used to
populate `threads-packages' with metadata about the packages Threads needs to keep
track of.

Only use this macro in a module's packages.el file.

Accepts the following properties:

 :recipe RECIPE        Takes a MELPA-style recipe (see `quelpa-recipe' in
                       `quelpa' for an example); for packages to be installed
                       from external sources.
 :pin ARCHIVE-NAME     Instructs ELPA to only look for this package in
                       ARCHIVE-NAME. e.g. \"org\". Ignored if RECIPE is present.
 :ignore FORM          Do not install this package if FORM is non-nil.
 :freeze FORM          Do not update this package if FORM is non-nil."
  (declare (indent defun))
  (let* ((old-plist (assq name threads-packages))
         (pkg-recipe (or (plist-get plist :recipe)
                         (and old-plist (plist-get old-plist :recipe))))
         (pkg-pin    (or (plist-get plist :pin)
                         (and old-plist (plist-get old-plist :pin)))))
    (when pkg-recipe
      (when (= 0 (% (length pkg-recipe) 2))
        (plist-put plist :recipe (cons name pkg-recipe)))
      (when pkg-pin
        (plist-put plist :pin nil)))
    (dolist (prop '(:ignore :freeze))
      (when-let* ((val (plist-get plist prop)))
        (plist-put plist prop (eval val))))
    `(progn
       (when ,(and pkg-pin t)
         (cl-pushnew (cons ',name ,pkg-pin) package-pinned-packages
                     :test #'eq :key #'car))
       (when ,(and old-plist t)
         (assq-delete-all ',name threads-packages))
       (push ',(cons name plist) threads-packages))))

(defmacro depends-on! (module submodule)
  "Declares that this module depends on another.

Only use this macro in a module's packages.el file.

MODULE is a keyword, and SUBMODULE is a symbol. Under the hood, this simply
loads MODULE SUBMODULE's packages.el file."
  (threads-module-enable module submodule)
  `(load! packages ,(threads-module-path module submodule) t))


;;
;; Commands
;;

(defun threads-packages--read-if-cookies (file)
  "Returns the value of the ;;;###if predicate form in FILE."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 256)
    (if (and (re-search-forward "^;;;###if " nil t)
             (<= (line-number-at-pos) 3))
        (let ((load-file-name file))
          (eval (sexp-at-point)))
      t)))

(defun threads-packages--async-run (fn)
  (let* ((default-directory threads-emacs-dir)
         (compilation-filter-hook
          (list (lambda () (ansi-color-apply-on-region compilation-filter-start (point))))))
    (compile (format "%s --quick --batch -l core/core.el -f %s"
                     (executable-find "emacs")
                     (symbol-name fn)))
    (while compilation-in-progress
      (sit-for 1))))

(defun threads//reload-load-path ()
  "Reload `load-path' and recompile files (if necessary).

Use this when `load-path' is out of sync with your plugins. This should only
happen if you manually modify/update/install packages from outside Emacs, while
an Emacs session is running.

This isn't necessary if you use Threads' package management commands because they
call `threads/reload-load-path' remotely (through emacsclient)."
  (interactive)
  (byte-recompile-file (expand-file-name "core.el" threads-core-dir) t)
  (cond (noninteractive
         (require 'server)
         (when (server-running-p)
           (message "Reloading active Emacs session...")
           (server-eval-at server-name '(threads//reload-load-path))))
        ((let ((noninteractive t))
           (threads-initialize-load-path t)
           (message "%d packages reloaded" (length threads--package-load-path))
           (run-hooks 'threads-reload-hook)))))

(defun threads//reload-autoloads ()
  "Refreshes the autoloads.el file, specified by `threads-autoload-file'.

It scans and reads core/autoload/*.el, modules/*/*/autoload.el and
modules/*/*/autoload/*.el, and generates an autoloads file at the path specified
by `threads-autoload-file'. This file tells Emacs where to find lazy-loaded
functions.

This should be run whenever init.el or an autoload file is modified. Running
'make autoloads' from the commandline executes this command."
  (interactive)
  ;; This function must not use autoloaded functions or external dependencies.
  ;; It must assume nothing is set up!
  (if (not noninteractive)
      ;; This is done in another instance to protect the current session's
      ;; state. `threads-initialize-packages' will have side effects otherwise.
      (and (threads-packages--async-run 'threads//reload-autoloads)
           (load threads-autoload-file))
    (threads-initialize-packages t)
    (let ((targets
           (file-expand-wildcards
            (expand-file-name "autoload/*.el" threads-core-dir))))
      (dolist (path (threads-module-paths))
        (let ((auto-dir  (expand-file-name "autoload" path))
              (auto-file (expand-file-name "autoload.el" path)))
          (when (file-exists-p auto-file)
            (push auto-file targets))
          (when (file-directory-p auto-dir)
            (dolist (file (directory-files-recursively auto-dir "\\.el$"))
              (push file targets)))))
      (when (file-exists-p threads-autoload-file)
        (delete-file threads-autoload-file)
        (message "Deleted old autoloads.el"))
      (dolist (file (reverse targets))
        (message
         (cond ((not (threads-packages--read-if-cookies file))
                "⚠ Ignoring %s")
               ((update-file-autoloads file nil threads-autoload-file)
                "✕ Nothing in %s")
               (t
                "✓ Scanned %s"))
         (file-relative-name file threads-emacs-dir)))
      (make-directory (file-name-directory threads-autoload-file) t)
      (let ((buf (get-file-buffer threads-autoload-file))
            current-sexp)
        (unwind-protect
            (condition-case-unless-debug ex
                (with-current-buffer buf
                  (save-buffer)
                  (goto-char (point-min))
                  (while (re-search-forward "^(" nil t)
                    (save-excursion
                      (backward-char)
                      (setq current-sexp (read (thing-at-point 'sexp t)))
                      (eval current-sexp t))
                    (forward-char))
                  (message "Finished generating autoloads.el!"))
              ('error
               (delete-file threads-autoload-file)
               (error "Error in autoloads.el: (%s %s ...) %s -- %s"
                      (nth 0 current-sexp)
                      (nth 1 current-sexp)
                      (car ex) (error-message-string ex))))
          (kill-buffer buf))))))

(defun threads//byte-compile (&optional modules recompile-p)
  "Byte compiles your emacs configuration.

init.el is always byte-compiled by this.

If MODULES is specified (a list of module strings, e.g. \"lang/php\"), those are
byte-compiled. Otherwise, all enabled modules are byte-compiled, including Threads
core. It always ignores unit tests and files with `no-byte-compile' enabled.

Threads was designed to benefit from byte-compilation, but the process may take a
while. Also, while your config files are byte-compiled, changes to them will not
take effect! Use `threads//clean-byte-compiled-files' or `make clean' to remove
these files.

If RECOMPILE-P is non-nil, only recompile out-of-date files."
  (interactive
   (list nil current-prefix-arg))
  (let ((default-directory threads-emacs-dir)
        (recompile-p (or recompile-p
                         (and (member "-r" (cdr argv)) t))))
    (if (not noninteractive)
        ;; This is done in another instance to protect the current session's
        ;; state. `threads-initialize-packages' will have side effects otherwise.
        (threads-packages--async-run 'threads//byte-compile)
      (let ((total-ok   0)
            (total-fail 0)
            (total-noop 0)
            (modules (or modules (cdr argv)))
            compile-targets)
        (threads-initialize-packages t t)
        (setq compile-targets
              (cl-loop for target
                       in (or modules (append (list threads-core-dir) (threads-module-paths)))
                       if (equal target "core")
                        nconc (nreverse (directory-files-recursively threads-core-dir "\\.el$"))
                       else if (file-directory-p target)
                        nconc (nreverse (directory-files-recursively target "\\.el$"))
                       else if (file-directory-p (expand-file-name target threads-modules-dir))
                        nconc (nreverse (directory-files-recursively (expand-file-name target threads-modules-dir) "\\.el$"))
                       else if (file-exists-p target)
                        collect target
                       finally do (setq argv nil)))
        (unless compile-targets
          (error "No targets to compile"))
        (let ((use-package-expand-minimally t))
          (push (expand-file-name "init.el" threads-emacs-dir) compile-targets)
          (condition-case ex
              (progn
                (dolist (target compile-targets)
                  (when (or (not recompile-p)
                            (let ((elc-file (byte-compile-dest-file target)))
                              (and (file-exists-p elc-file)
                                   (file-newer-than-file-p file elc-file))))
                    (let ((result (if (threads-packages--read-if-cookies target)
                                      (byte-compile-file target)
                                    'no-byte-compile))
                          (short-name (file-relative-name target threads-emacs-dir)))
                      (cl-incf
                       (cond ((eq result 'no-byte-compile)
                              (message! (dark (white "⚠ Ignored %s" short-name)))
                              total-noop)
                             ((null result)
                              (message! (red "✕ Failed to compile %s" short-name))
                              total-fail)
                             (t
                              (message! (green "✓ Compiled %s" short-name))
                              (quiet! (load target t t))
                              total-ok))))))
                (message!
                 (bold
                  (color (if (= total-fail 0) 'green 'red)
                         "%s %s file(s) %s"
                         (if recompile-p "Recompiled" "Compiled")
                         (format "%d/%d" total-ok (- (length compile-targets) total-noop))
                         (format "(%s ignored)" total-noop)))))
            (error
             (message! (red "\n%%s\n\n%%s\n\n%%s")
                       "There were breaking errors."
                       (error-message-string ex)
                       "Reverting changes...")
             (threads//clean-byte-compiled-files)
             (message! (green "Finished (nothing was byte-compiled)")))))))))

(defun threads//byte-compile-core (&optional recompile-p)
  "Byte compile the core Threads files.

This is faster than `threads//byte-compile', still yields considerable performance
benefits, and is more reliable in an ever-changing Emacs config (since you won't
likely change core files directly).

If RECOMPILE-P is non-nil, only recompile out-of-date core files."
  (interactive "P")
  (if (not noninteractive)
      ;; This is done in another instance to protect the current session's
      ;; state. `threads-initialize-packages' will have side effects otherwise.
      (threads-packages--async-run 'threads//byte-compile-core)
    (threads//byte-compile (list "core") recompile-p)))

(defun threads//byte-recompile-plugins ()
  "Recompile all installed plugins. If you're getting odd errors after upgrading
(or downgrading) Emacs, this may fix it."
  (interactive)
  (byte-recompile-directory package-user-dir 0 t))

(defun threads//clean-byte-compiled-files ()
  "Delete all the compiled elc files in your Emacs configuration. This excludes
compiled packages.'"
  (interactive)
  (let ((targets (append (list (expand-file-name "init.elc" threads-emacs-dir))
                         (directory-files-recursively threads-core-dir "\\.elc$")
                         (directory-files-recursively threads-modules-dir "\\.elc$")))
        (default-directory threads-emacs-dir))
    (unless (cl-loop for path in targets
                     if (file-exists-p path)
                     collect path
                     and do (delete-file path)
                     and do (message "✓ Deleted %s" (file-relative-name path)))
      (message "Everything is clean"))))


;;
;; Package.el modifications
;;

;; Updates QUELPA after deleting a package
(advice-add #'package-delete :after #'threads*package-delete)

;; It isn't safe to use `package-autoremove', so get rid of it
(advice-add #'package-autoremove :override #'threads//packages-autoremove)

(provide 'core-packages)
;;; core-packages.el ends here
