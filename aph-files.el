;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; FILE-HANDLING EXTENSIONS
;;;;============================================================================

;; Extensions for `files' package.


;;; Make Emacs Source Read-Only
;;;============================
;; This code makes Emacs source code default to read-only mode.
;; Obtained from a stackexchange answer by user phils. 
(defvar aph/emacs-source-dirs
  '("/usr/local/src/emacs"
    "/usr/local/share/emacs"
    "/usr/share/emacs")
  "List of directories containing Emacs source.
Referenced by `aph/emacs-source-make-read-only'.")

(defun aph/emacs-source-make-read-only ()
  "Make Emacs source code open in read-only mode by default.
Affects only built-in Emacs files and ELPA packages, not init
files or other user code."
  (dir-locals-set-class-variables
   'default '((nil . ((buffer-read-only . nil))))) 
  (dir-locals-set-class-variables
   'emacs '((nil . ((buffer-read-only . t)))))

  ;; Built-in source
  (mapcar (lambda (dir)
            (dir-locals-set-directory-class dir 'emacs))
          aph/emacs-source-dirs)

  ;; Package code is a special problem, because these buffers need to be
  ;; writable for `package-install'.
  (dir-locals-set-directory-class package-user-dir 'emacs)
  (advice-add #'package-install :around #'aph/package-install-writability))

(defun aph/package-install-writability (orig-fn &rest args)
  "Advice so `package-install' can write to ELPA directory.
Intended as :around advice for `package-install'."
  (dir-locals-set-directory-class package-user-dir 'default)
  (apply orig-fn args)
  (dir-locals-set-directory-class package-user-dir 'emacs)) 


(provide 'aph-files)
