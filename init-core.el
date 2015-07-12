;;;; The Emacs init files of Aaron Harris:
;;;; CORE FILE
;;;;============================================================================

(message "Beginning initialization process...") 


;;; Bootstrapping Variables
;;;========================
;; These variables should have been set by the bootstrapper.
(defvar aph/init-path "~/sync/emacs-init"
  "The path to the directory containing my init files.")

(defvar aph/machine 'default
  "A symbol denoting the specific PC being used.")


;;; Path Management
;;;================
(add-to-list 'load-path (expand-file-name aph/init-path))
(add-to-list 'load-path "~/.emacs.d/lisp")


;;; Initialization Functions
;;;=========================
;; These are functions which are used directly by these initialization files.
(defun aph/add-hook-safely (hook function &optional append local)
  "As `add-hook', but check if FUNCTION is defined first.

If FUNCTION is defined, proceed to add it to HOOK. If it isn't,
print a message to that effect and do nothing.

The return value is HOOK if it was added and nil otherwise."
  (if (fboundp function)
      (add-hook hook function append local)
    (message "Cannot add #'%s to %s: Function not defined"
             function hook)
    nil)) 

(defun aph/add-hook-to-all (hooks function &optional safely)
  "Add FUNCTION to each hook in the list HOOKS, with `add-hook'.

If the optional parameter SAFELY is supplied, use
`aph/add-hook-safely' instead of `add-hook'."
  (dolist (hook hooks)
    (if safely
        (aph/add-hook-safely hook function)
      (add-hook hook function))))

(defun aph/require-softly (feature &optional filename)
  "As `require', but instead of an error just print a message.

If there is an error, its message will be included in the message
printed.

Like `require', the return value will be FEATURE if the load was
successful (or unnecessary) and nil if not."
  (condition-case err
      (require feature filename) 
    (error (message (concat "Error loading %s"
                            (if filename " (%s): \"" ": \"")
                            (error-message-string err)
                            "\"")
                    feature filename)
           nil)))


;;; Loading Submodules
;;;===================
(aph/require-softly 'init-package)

;; Major Features
(aph/require-softly 'init-gnus)
(aph/require-softly 'init-org)
(aph/require-softly 'init-smartparens)

;; Specific Modes
(aph/require-softly 'init-docview)
(aph/require-softly 'init-latex)
(aph/require-softly 'init-lisp)

;; Other
(aph/require-softly 'init-misc) 
(aph/require-softly 'init-theme)
(aph/require-softly 'init-keys)

;; Remember to sort out aph-geog.el. Need to do this at work.

(message "Initialization complete!")
(message "-----------------------------------")

(provide 'init-core)
(provide 'init)
