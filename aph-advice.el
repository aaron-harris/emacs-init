;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ADVICE FUNCTIONS
;;;;============================================================================

;;; This file contains functions interacting with the Emacs advice
;;; facility.


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
present, the only keyword recognized is :once, which indicates that
the advice should be applied using `aph/advice-once' so that it is
executed at most once.

The BODY is wrapped in an `unwind-protect' form, so the advice
will be removed even in the event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1)) 
  `(progn
     ,@(mapcar
        (lambda (adform)
          (let ((opt (if (keywordp (car adform)) (car adform) nil)))
            (cond
             ((null opt)     (cons 'advice-add adform)) 
             ((eq opt :once) (cons 'aph/advice-once (cdr adform))) 
             (t              (error "Unrecognized option %s" opt)))))
        adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar
          (lambda (adform)
            (let ((opt (if (keywordp (car adform)) (car adform) nil)))
              (cond
               ((null opt) `(advice-remove ,(car adform) ,(nth 2 adform)))
               (t          `(advice-remove ,(nth 1 adform) ,(nth 3 adform))))))
          adlist))))


;;; Utility Functions
;;;==================
(defun aph/advice-clear (symbol)
  "Remove all advice on SYMBOL."
  (advice-mapc (lambda (fn props) (advice-remove symbol fn))
               symbol))

(provide 'aph-advice)
