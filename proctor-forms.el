;;; proctor-forms.el --- ERT support for `forms-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: tools, lisp

;; Dependencies: `proctor', `forms', `bfw'

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

;; This module contains macros and functions designed for testing code
;; interacting with `forms-mode'.

;;; Code:

(require 'proctor)
(require 'forms)

(require 'bfw)


;;;; Fixtures
;;===========
(defun proctor-forms--encode-db (&rest records)
  "Encode RECORDS as the content of a forms-mode database.

This is a helper function for the macro `proctor-forms-with-db'.

At present no support is provided for non-standard field or
record separators."
  (mapconcat (lambda (fields)
               (mapconcat #'identity fields "\t"))
             records "\n"))

(defmacro proctor-forms-with-db (init-form records &rest body)
  "Execute BODY in `forms-mode' db containing RECORDS.

More precisely, this macro does the following:

* Create a `forms-mode' control file that just sets the variable
  `forms-file' and evaluates INIT-FORM.

* Create the corresponding `forms-mode' database file, and insert
  the contents of RECORDS in that file.  See below for a
  description of how the RECORDS parameter should be formatted.

* Call `forms-find-file' on the control file to open the
  database, and evaluate BODY inside the resulting buffer.

* After BODY has finished, delete all files created and kill all
  associated buffers.


The RECORDS parameter should be a list.  Each element of this
list should itself be a list, representing a single record in the
database, and the elements of these lists should be strings,
representing the fields of that record.  All element lists should
have the same length (since all records in a `forms-mode'
database have the same number of fields).

Example usage:

    (proctor-forms-with-db
     (setq forms-modified-record-filter (...))
     ((\"1\" \"foo\")
      (\"2\" \"\")
      (\"3\" \"bar\"))
     (should ...))"
  (declare (debug (form sexp body))
           (indent 2))
  (let ((db-buffer  (make-symbol "db-buffer")))
    `(let (,db-buffer)
       (require 'warnings)
       (proctor-with-file "test.ctrl"
           ,(format "(setq forms-file \"test.db\")\n%S" init-form)
         (let ((warning-suppress-log-types
                (cons '(undo discard-info) warning-suppress-log-types)))
           (proctor-with-file "test.db"
               ,(apply #'proctor-forms--encode-db records)
             (forms-find-file (expand-file-name "test.ctrl" proctor-directory))
             (setq ,db-buffer (current-buffer))
             (unwind-protect (progn ,@body)
               (bfw-kill-buffer-nowarn ,db-buffer))))))))

(provide 'proctor-forms)
;;; proctor-forms.el ends here
