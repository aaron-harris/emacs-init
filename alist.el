;;; alist.el --- A better alist interface            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: extensions, alist

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

;; Some operations on alists are much more laborious than they should
;; be, most notably the process of updating a key that may or may not
;; be present.  In order to set `key' to `val' in a variable `alist',
;; you have to do something like this:
;;
;;     (let ((elt (assq key alist)))
;;       (if elt (setf (cdr elt) val)
;;         (setq alist (push `(,key . ,val) alist))))
;;
;; The `map' library for Emacs 25 neatly solves this
;; problem, turning the above code into this:
;;
;;     (map-put alist key val)
;;
;; Unfortunately, this library is not available for Emacs 24.  This
;; module is intended as a partial stopgap measure, providing
;; functions to solve the most egregious of these interface problems,
;; at least in alists.  Unlike the `map' module, no attempt is made to
;; support hash maps or arrays.

;;; Code:

(require 'cl-lib)


;;;; Updating
;;===========
(defmacro alist-delete (alist key &optional test)
  "Delete association in ALIST for KEY.  Return ALIST.

If the optional parameter TEST is supplied, it is used in place
of `eql' to compare elements.

Here, ALIST may be any generalized variable containing an
alist."
  (declare (debug (gv-place form &optional function-form)))
  (let ((test (or test '#'eql)))
    `(setf ,alist
           (cl-delete ,key ,alist :test ,test :key #'car))))

(defun alist--put (alist key value &optional test)
  "Subroutine used by `alist-put'.

As `alist-put', except back-assignment may be necessary, as with
`delete'." 
  (let* ((test  (or test #'eql))
         (elt   (cl-assoc key alist :test test)))
    (if elt (setf (cdr elt) value)
      (setq alist (push `(,key . ,value) alist)))
    alist))

(defmacro alist-put (alist key value &optional test)
  "Associate KEY with VALUE in ALIST.  Return ALIST.

If ALIST does not already contain an association for KEY, it is
added; otherwise, the existing association is updated.

If the optional parameter TEST is supplied, it is used in place
of `eql' to compare elements.

Here, ALIST may be any generalized variable containing an alist."
  (declare (debug (gv-place form form &optional function-form)))
  `(setf ,alist (alist--put ,alist ,key ,value ,test)))


;;;; Equality Testing
;;===================
(defun alist-equal (alist1 alist2 &optional key-test value-test default)
  "Return non-nil if ALIST1 and ALIST2 are equal as alists.

Two alists are considered equal if the values for corresponding
keys (where equality is determined by KEY-TEST) are equal
according to VALUE-TEST.  Most notably, keys may appear in any
order.

If KEY-TEST is omitted, it defaults to `eq', while if VALUE-TEST
is omitted, it defaults to `equal'.  This is consistent with the
most common use of alists, in which keys are symbols but values
may be a wide range of types.

If a key appears in one list but not the other, then DEFAULT will
be used for its value in the list where it does not appear."
  (setq key-test   (or key-test   #'eq)
        value-test (or value-test #'equal))
  (catch 'fail
    (while (not (null alist1))
      (let* ((elt  (pop alist1))
             (k1   (car elt))
             (v1   (cdr elt))
             (v2   (or (cdr (cl-assoc k1 alist2 :test key-test))
                       default)))
        (if (funcall value-test v1 v2)
            (alist-delete alist2 k1 key-test)
          (throw 'fail nil))))
    (if (null alist2) t
      (alist-equal alist2 alist1 key-test value-test default))))

(provide 'alist)
;;; alist.el ends here
