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
;; To select the field shown by `helm-forms-records' as the record
;; title, use the variable `helm-forms-name-field'.  By default, the
;; first field is used.

;;; Code:

(require 'formation)
(require 'helm)

(defvar-local helm-forms-name-field 1
  "Field number containing the name of the current record.
This is used as a description by `helm-forms-records'.")

(defun helm-forms-record-candidates ()
  "Compile a list of records in current database for Helm."
  (with-helm-current-buffer
    (formation-map
     (lambda ()
       `(,(nth helm-forms-name-field forms-fields)
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
