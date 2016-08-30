;;; random.el --- Random selection functions -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: lisp

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

;; This module defines functions and macros that allow for
;; higher-level selection of random outcomes than the built-in
;; `random' primitive.  These include:
;;
;; `random-dispatch'
;;
;;     This macro takes (modulo some syntactic sugar) an alist
;;     associating integer weights to elisp forms and randomly
;;     evaluates one of the forms.
;;
;; `random-dispatch*'
;;
;;     This function is just like `random-dispatch', but without the
;;     syntax sugar.

;;; Code:


;;;; Random dispatch
;;==================
(defun random-dispatch* (alist)
  "Evaluate a random form from ALIST.

ALIST should be an association list with integer keys.  Those
keys are interpreted as weights for the random selection of a
value from the ALIST.  The selected value is then evaluated as an
elisp form."
  (let* ((counter  (lambda (acc pair) (+ acc (car pair))))
         (total    (seq-reduce counter alist 0))
         (roll     (random total)))
    (catch 'hit
      (seq-reduce
       (lambda (acc pair)
         (setq acc (funcall counter acc pair))
         (if (< roll acc)
             (throw 'hit (eval (cdr pair)))
           acc))
       alist 0))))

(defmacro random-dispatch (&rest lists)
  "Evaluate a random form from LISTS.

Each LIST should start with an integer, and the remaining
elements should be elisp forms.  Interpret the integers as
weights for the random selection of one LIST, and evaluate all
forms in that LIST in order.  Return the value of the last form
evaluated."
  (declare (debug (&rest (sexp body))))
  `(random-dispatch*
    ',(mapcar (lambda (list)
                `(,(car list) . (progn ,@(cdr list))))
              lists))) 

(provide 'random)
;;; random.el ends here
