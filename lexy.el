;;; lexy.el --- Tools for working with lexical scope -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: extensions

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

;; This module contains functions for distinguishing between lexical
;; and dynamic scope, and for changing what kind of scoping applies to
;; particular code.

;;; Code:


;;;; Macros
;;=========
;;;###autoload
(defmacro define-dynamically (&rest def)
  "As `defun', but always in dynamic scope.
A function defined in this way ignores the value of
`lexical-binding' and treats it as if it were nil.

\(fn NAME ARGLIST &optional DOCSTRING DECL &rest BODY)"
  (declare (debug defun)
           (indent defun)
           (doc-string 3))
  `(eval '(defun ,@def) nil))


;;;; Utility Functions
;;====================

;; Taken from the blog post
;;
;; https://yoo2080.wordpress.com/2013/09/11/emacs-lisp-lexical-binding-gotchas-and-related-best-practices/

(defmacro lexy-lexical-p (var)
  "Returns t if VAR can be lexically bound, and nil otherwise.

Specially, this will return nil when called in dynamic scope, and
it will return nil if var has been declared as a special
variable (e.g., with `defvar').  All other cases should return
t."
  `(let ((,var nil)
         (f (let ((,var t)) (lambda () ,var))))
     (funcall f)))

(provide 'lexy)
;;; lexy.el ends here
