;;;; The Emacs init file of Aaron Harris.
;;;; LISP CONFIGURATION
;;;;============================================================================


;;; Mode-Bundling Setup
;;;==================== 
(defvar aph/lisp-mode-hooks
  '(cider-repl-mode-hook
    clojure-mode-hook
    emacs-lisp-mode-hook
    eval-expression-minibuffer-setup-hook
    ielm-mode-hook
    lisp-interaction-mode-hook
    lisp-mode-hook)
  "A list of hooks for modes in which Lisp editing occurs.

Can be used with `aph/add-hook-to-all' to easily add a hook to all such modes.")


;;; Using Specific Lisps
;;;=====================
(aph/require-softly 'init-lisp-clojure)


;;; All-Lisp Minor Modes
;;;=====================
(aph/add-hook-to-all aph/lisp-mode-hooks #'smartparens-strict-mode :safely)
(aph/add-hook-to-all aph/lisp-mode-hooks #'rainbow-delimiters-mode :safely) 


;;; Make Emacs Source Read-Only
;;;============================
;; This code makes Emacs source code default to read-only mode.
;; Obtained from a stackexchange answer by user phils. 

(dir-locals-set-class-variables
 'default
 '((nil . ((buffer-read-only . nil)))))

(dir-locals-set-class-variables
 'emacs
 '((nil . ((buffer-read-only . t)))))

(if (eq aph/machine 'mpc)
    (dir-locals-set-directory-class
     "C:/Program Files (Portable)/Emacs/share/emacs" 'emacs)
  (dir-locals-set-directory-class "/usr/local/src/emacs"   'emacs)
  (dir-locals-set-directory-class "/usr/local/share/emacs" 'emacs)
  (dir-locals-set-directory-class "/usr/share/emacs"       'emacs))

;; Package code is a special problem, because these buffers need to be
;; writable for `package-install'.
(dir-locals-set-directory-class "~/.emacs.d/elpa" 'emacs) 

(defun aph/package-install-writability (package-install &rest args)
  "Advice so `package-install' can write to elpa directory."
  (dir-locals-set-directory-class "~/.emacs.d/elpa" 'default)
  (apply package-install args)
  (dir-locals-set-directory-class "~/.emacs.d/elpa" 'emacs)) 
(advice-add #'package-install :around #'aph/package-install-writability) 

(provide 'init-lisp)
