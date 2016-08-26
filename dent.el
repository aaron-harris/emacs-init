;;; dent.el --- Custom elisp indentation functions   -*- lexical-binding: t; -*-

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

;; This module contains general-use Elisp indentation functions.

;;; Code:

(defun dent-indent-specform (indent-point state)
  "Indentation function for macros with several distinguished args.

The desired indentation is like `(declare (indent N))', except
all of the first N arguments are indented to double the body
indentation, not just the first two.

The value for N is taken from the `dent-indent-function-spec'
property associated with the function symbol for the sexp being
indented.  E.g., to use this function and indent the first three
arguments, you would write:

    (defmacro foo (&rest args)
      \"Foo!\"
      (declare (indent dent-indent-specform))
      (...))

    (put 'foo 'dent-indent-function-spec 3)"
  ;; This code is mostly copied directly from `lisp-indent-specform'.
  (let* ((normal-indent (current-column))
         (containing-form-start (elt state 1))
         (count (or (get (symbol-at-point) 'dent-indent-function-spec) 0))
         (i count)
         body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  lisp-indent-function guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ lisp-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (if (> count 0)
        ;; A distinguished form.  Use double lisp-body-indent.
        (list (+ containing-form-column (* 2 lisp-body-indent))
              containing-form-start)
      ;; A non-distinguished form.  Use body-indent.
      body-indent)))

(provide 'dent)
;;; dent.el ends here
