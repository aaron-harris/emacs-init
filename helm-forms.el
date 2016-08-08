;;; helm-forms.el --- Helm  for `forms-mode'         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience data

;; Dependencies: `formation', `helm'

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

;; This module defines one command for `helm', `helm-forms-records'.
;; This command, when called from a `forms-mode' database, allows you
;; to browse all records in the database and jump to any one of them.
;;
;; To change which field is displayed as the record's name, set the
;; variable `helm-forms-name-field'; the default is the first field.
;; To use a string which is not stored in any single field, set
;; `helm-forms-name-function' to any custom function.

;;; Code:

(require 'formation)
(require 'helm)

(defvar-local helm-forms-name-function
  #'helm-forms-get-name-from-field
  "Function used to get the name of the current record.
Used by `helm-forms-records'.

This function should return the name of the current record,
typically by examining `forms-fields'.  It is called with no
arguments.

If the name is stored in a single field, leave this variable at
the default (`helm-get-name-from-field') and set the variable
`helm-forms-name-field' to the desired field number.")

(defvar-local helm-forms-name-field 1
  "Field number containing the name of the current record.
Used by `helm-forms-get-name-from-field'.

If there is no single field containing the desired name, you
should change `helm-forms-name-function' instead of this
variable.")

(defun helm-forms-get-name-from-field ()
  "Return the string in field number `helm-forms-name-field'.

This is the default implementation for `helm-forms-name-function'
and is used by `helm-forms-records' to generate record names."
  (nth helm-forms-name-field forms-fields))

(defun helm-forms-record-candidates ()
  "Compile a list of records in current database for Helm."
  (with-helm-current-buffer
    (formation-map
     (lambda ()
       `(,(funcall helm-forms-name-function)
         . ,forms--current-record)))))

(defvar helm-source-forms-records
  (helm-build-sync-source "Records"
    :candidates #'helm-forms-record-candidates
    :action     #'forms-jump-record))

;;;###autoload
(defun helm-forms-records ()
  "A `helm' command for browsing records in `forms-mode'."
  (interactive)
  (helm :sources '(helm-source-forms-records)))

(provide 'helm-forms)
;;; helm-forms.el ends here
