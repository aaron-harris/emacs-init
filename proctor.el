;;; proctor.el --- Test apparatus for use with ERT   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: tools, lisp

;; Dependencies: `ert-x'

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

;; This module contains macros and functions for general-purpose use
;; inside ERT tests, as fixtures, mocks, etc.

;;; Code:

(require 'ert-x)
(eval-when-compile (require 'subr-x))


;;;; Macro Testing
;;================
(defun proctor-macro-executes-body (macro &optional other-args)
  "Execute a standard test to check that MACRO executes a body.

Apply MACRO to a body form, prepending any OTHER-ARGS, and check
both that this body was executed, and that the value returned is
the return value of its last form.  Return t if both conditions
hold, and signal an error with `should' otherwise."
  (let* ((canary  (make-symbol "canary"))
         (test    `(let (,canary)
                     (should (= 7 (,macro ,@other-args
                                          (setq ,canary t)
                                          (+ 1 6))))
                     (should ,canary))))
    (eval test)))

(defun proctor-macro-does-not-leak (macro var-form &optional other-args)
  "Test to ensure that MACRO does not leak binding for VAR-FORM.

Apply MACRO to a body form, prepending any OTHER-ARGS, and check
that the symbol whose name is given by VAR-FORM inside BODY is
not defined as either a variable or a function after BODY exits.

Note that it is acceptable for VAR-FORM to name a symbol that is
either `boundp' or `fboundp', so long as it is not interned."
  (let* ((var-x  (make-symbol "var-x"))
         (subtest
          (lambda (wrap form)
            `(let (,var-x)
               (,wrap
                (,macro ,@other-args
                        (setq ,var-x ,var-form)
                        ,form)
                (should-not (and (intern-soft ,var-x)
                                 (or (boundp ,var-x) (fboundp ,var-x)))))))))
    (eval (funcall subtest 'progn         '(ignore)))
    (eval (funcall subtest 'ignore-errors '(error "Triggered error"))) 
    t))


;;;; Buffer Handling
;;==================
(defmacro proctor-with-buffer (mode text &rest body)
  "Execute BODY in temp buffer in MODE that contains TEXT.

Create a temporary buffer, insert TEXT, and enable
MODE (typically a major mode).  Move point to the beginning of
this buffer and execute BODY.

The buffer is created with `ert-with-test-buffer', so it will
persist in the event of an error in BODY.

For convenience, if TEXT begins with a newline, that newline is
not included in the buffer text.  This allows for the following
sort of layout, which avoids both problematic indentation and the
need to skip over the leading newline.

  (proctor-with-buffer text-mode \"
Buffer line 1
Buffer line 2\"
    (should (looking-at-p \"Buffer line 1\")))"
  (declare (indent 2)
           (debug t))
  `(ert-with-test-buffer ()
     (insert (string-remove-prefix "\n" ,text))
     (funcall ,mode)
     (goto-char (point-min))
     ,@body))

(provide 'proctor)
;;; proctor.el ends here
