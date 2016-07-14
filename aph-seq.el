;;; aph-seq.el --- Extensions to `seq' library       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: extensions, sequences

;; Dependencies: `seq'

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

;; This module contains additional sequence-manipulation functions
;; extending those in the `seq' library.

;;; Code:

(require 'seq)
(eval-when-compile (require 'dash))


;;;; Type Introspection
;;=====================
(defun aph/seq-type (sequence)
  "Return the type of SEQUENCE.

This is a symbol: vector, string, or list.  If SEQUENCE is not
one of these types, an error is signaled."
  (cond
   ((listp sequence)   'list)
   ((stringp sequence) 'string)
   ((vectorp sequence) 'vector)
   (:else              (signal 'wrong-type-argument `(seq-p ,sequence)))))


;;;; Search
;;=========
(defun aph/seq-successor (sequence elt &optional testfn circular)
  "If ELT is in SEQUENCE, return its successor; else, nil.

As with `seq-contains', equality is defined by TESTFN if non-nil
and defaults to `equal'.

If ELT appears multiple times in SEQUENCE, return the successor
of its first occurrence.

If the CIRCULAR parameter is non-nil, treat SEQUENCE as circular:
if ELT is the last element of SEQUENCE, return the first element
of SEQUENCE instead of nil."
  (let* ((testfn (or testfn #'equal))
         (tail   (seq-drop-while
                  (lambda (x) (not (funcall testfn elt x)))
                  sequence)))
    (or (seq--elt-safe tail 1)
        (and circular tail (elt sequence 0))
        nil)))


;;;; Reduction
;;============
(defun aph/seq-reductions (function sequence initial-value)
  "Return sequence of partial results from `seq-reduce'.

The first element of the returned sequence is INITIAL-VALUE, the
second element is the result of calling FUNCTION with
INITIAL-VALUE and the first element of SEQUENCE, and so forth.
The last element is the value that would be returned by
`seq-reduce' if it were called with these arguments." 
  (-> (lambda (acc val)
        (cons (funcall function (car acc) val)
              acc))
      (seq-reduce sequence (list initial-value))
      reverse
      (seq-into (aph/seq-type sequence))))

(provide 'aph-seq)
;;; aph-seq.el ends here
