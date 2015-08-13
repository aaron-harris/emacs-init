;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ELISP CONFIGURATION
;;;;============================================================================


;;; Output Format Control
;;;======================
;; This function was taken from a stackexchange answer by user Harald
;; Hanche-Olsen.  I have subsequently reformatted it to match my code
;; style.
(defvar aph/eval-expression-clean-output nil
  "If non-nil, evaluating elisp will not return extra info.

Normally, evaluating elisp (e.g., via `eval-expression' or
`ielm') will format its output with extraneous data, such as:
  (+ 1 1)
  ;=> 2 (#o2, #x2, ?\\C-b) 
This output is produced by the function
`eval-expression-print-format'.

When this variable is non-nil, all output from
`eval-expression-print-format' is silenced (so the previous
example would just return 2.")

(defun aph/eval-expression-toggle-clean-output ()
  "Toggle the variable `aph/eval-expression-clean-output'."
  (interactive)
  (setq aph/eval-expression-clean-output
        (not aph/eval-expression-clean-output)))

(defun aph/mute-eval-expression-print-format (orig-fun value)
  "Advice to conditionally silence `eval-expression-print-format'.
See `aph/eval-expression-clean-output' for more information."
  (if aph/eval-expression-clean-output
      ""
    (funcall orig-fun value)))

(advice-add 'eval-expression-print-format
            :around #'aph/mute-eval-expression-print-format)

;; At work, we want this to be enabled by default.
(when (eq aph/machine 'mpc)
  (setq aph/eval-expression-clean-output t))


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

(provide 'init-lisp-emacs)
