;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ADVICE FUNCTIONS
;;;;============================================================================

;;; This file contains functions interacting with the Emacs advice
;;; facility.


;;; Temporary Advice
;;;=================
;; Functions and macros in this section install temporary advice.

(defmacro aph/with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))

(defun aph/advice-once (symbol where function &optional props)
  "As `advice-add', but remove advice after first call.

This can be useful to avoid infinite recursion, in the event that
FUNCTION calls the function named by SYMBOL directly." 
  (let* ((id      (cl-gensym "aph/advice-once:cleanup-"))
         (cleanup (lambda (&rest args)
                    (advice-remove symbol function)
                    (advice-remove symbol id))))
    (advice-add symbol where function props) 
    (advice-add symbol :before cleanup `((name . ,id)))))


;;; Utility Functions
;;;==================
(defun aph/advice-clear (symbol)
  "Remove all advice on SYMBOL."
  (advice-mapc (lambda (fn props) (advice-remove symbol fn))
               symbol))

(provide 'aph-advice)
