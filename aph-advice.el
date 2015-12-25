;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ADVICE FUNCTIONS
;;;;============================================================================

;;; This file contains functions interacting with the Emacs advice
;;; facility.


;;; Adform Manipulation
;;;====================
;; Functions in this section manipulate lists with a structure
;; appropriate for use as argument lists for `advice-add'.  These are
;; mostly for internal use by subsequent functions.
(defun aph/advice-genname (props)
  "Use `cl-gensym' to generate a generic 'name for PROPS.
Return a new alist incorporating this association.

The parameter PROPS is an alist.  Return the alist obtained from
PROPS by associating with the 'name key an uninterned symbol
obtained using `cl-gensym'.  The prefix used will be the existing
value for 'name in PROPS if one exists (appending a colon for
legibility), and \"aph/advice:\" otherwise."
  (let* ((adname    (assoc-default 'name props))
         (prefix  (format "%s:" (symbol-name (or adname 'aph/advice)))))
    (cons `(name . ,(cl-gensym prefix)) props)))


;;; Temporary Advice
;;;=================
;; Functions and macros in this section install temporary advice.
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

(defmacro aph/with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  ([OPTION] SYMBOL WHERE FUNCTION [PROPS]).
Here SYMBOL, WHERE, FUNCTION, and PROPS are as in `advice-add', and
OPTION is a keyword modifying how the advice should be handled.  At
present, the following keywords are recognized, with the specified
effects:

:once
This keyword indicates that the advice should be applied using
`aph/advice-once' so that it is executed at most once.

:genname
This keyword uses `cl-gensym' to generate a unique name for the
advice.  This prevents multiple advice that would otherwise be
identical from overriding one another.


The BODY is wrapped in an `unwind-protect' form, so the advice
will be removed even in the event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  (let ((removal-list nil))
    `(progn
       ,@(mapcar
          (lambda (adform)
            (let ((option (if (keywordp (car adform)) (pop adform) nil)))
              (cl-destructuring-bind
                  (symbol where function &optional props) adform
                (cond
                 ((null option)
                  (push `(advice-remove ,symbol ,function) removal-list)
                  (cons 'advice-add adform))

                 ((eq option :once)
                  (push `(advice-remove ,symbol ,function) removal-list)
                  (cons 'aph/advice-once (cdr adform)))

                 ((eq option :genname)
                  (let* ((props  (aph/advice-genname props))
                         (name   (assoc-default 'name props)))
                    (push `(advice-remove ,symbol ',name) removal-list)
                    `(advice-add ,symbol ,where ,function ',props)))))))
          adlist) 
       (unwind-protect (progn ,@body)
         ,@(reverse removal-list)))))


;;; Utility Functions
;;;==================
(defun aph/advice-clear (symbol)
  "Remove all advice on SYMBOL."
  (advice-mapc (lambda (fn props) (advice-remove symbol fn))
               symbol))

(provide 'aph-advice)
