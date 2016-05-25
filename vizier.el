;;; vizier.el --- Temporary advice                   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: extensions lisp tools advice

;; Dependencies: `cl-lib'

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module contains code that uses the advice functionality found
;; in `nadvice' to implement more sophisticated forms of "code
;; surgery".  In particular, this module supports using advice to
;; temporarily modify a function and thereby reuse other packages'
;; code without globally advising their functions.
;;
;; The principal tool provided by `vizier' is the macro
;; `vizier-with-advice'.  This will install specified advice, execute
;; a body, and then remove that advice.  It also has some other bells
;; and whistles; see its docstring for more information.
;;
;; More straightforward applications can use `vizier-advise-once' to
;; install self-removing advice that will only be executed once.  Be
;; careful with this--if the function being advised is not run when
;; you expect it, you can create a lingering "time bomb" that can
;; change some subsequent invocation in unexpected ways.
;;
;; Finally, there is the convenience function `vizier-clear', which
;; just removes all advice on a specified symbol.

;;; Code:

(require 'cl-lib)           ; For `cl-destructuring-bind', `cl-gensym'


;;;; Subroutines
;;============== 
(defun vizier--genname (props)
  "Use `cl-gensym' to generate a generic 'name for PROPS.
Return a new alist incorporating this association.

The parameter PROPS is an alist.  Return the alist obtained from
PROPS by associating with the 'name key an uninterned symbol
obtained using `cl-gensym'.  The prefix used will be the existing
value for 'name in PROPS if one exists (appending a colon for
legibility), and \"vizier:\" otherwise."
  (let* ((adname  (assoc-default 'name props))
         (prefix  (format "%s:" (symbol-name (or adname 'vizier)))))
    (cons `(name . ,(cl-gensym prefix)) props)))


;;;; Temporary Advice
;;=================== 
(defun vizier-advise-once (symbol where function &optional props)
  "As `advice-add', but remove advice after first call.

This can be useful to avoid infinite recursion, in the event that
FUNCTION calls the function named by SYMBOL directly." 
  (let* ((id      (cl-gensym "vizier-advise-once:cleanup-"))
         (cleanup (lambda (&rest args)
                    (advice-remove symbol function)
                    (advice-remove symbol id))))
    (advice-add symbol where function props) 
    (advice-add symbol :before cleanup `((name . ,id)))))

(defmacro vizier-with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  ([OPTION] SYMBOL WHERE FUNCTION [PROPS]).
Here SYMBOL, WHERE, FUNCTION, and PROPS are as in `advice-add', and
OPTION is a keyword modifying how the advice should be handled.  At
present, the following keywords are recognized, with the specified
effects:

:once
This keyword indicates that the advice should be applied using
`vizier-advise-once' so that it is executed at most once.

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
                  (push `(advice-remove ',symbol ,function) removal-list)
		  `(advice-add ',symbol ,where ,function ',props))

                 ((eq option :once)
                  (push `(advice-remove ',symbol ,function) removal-list)
		  `(vizier-advise-once ',symbol ,where ,function ',props))

                 ((eq option :genname)
                  (let* ((props  (vizier--genname props))
                         (name   (assoc-default 'name props)))
                    (push `(advice-remove ',symbol ',name) removal-list)
                    `(advice-add ',symbol ,where ,function ',props)))))))
          adlist) 
       (unwind-protect (progn ,@body)
         ,@removal-list))))


;;;; Utility Functions
;;====================
(defun vizier-clear (symbol)
  "Remove all advice on SYMBOL."
  (advice-mapc (lambda (fn props) (advice-remove symbol fn))
               symbol))

(provide 'vizier)
;;; vizier.el ends here
