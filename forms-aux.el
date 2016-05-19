;;; forms-aux.el --- Per-record files and buffers    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: data forms

;; Dependencies: `forms'

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

;; This module provides a mechanism by which `forms-mode' databases
;; can display a per-record "auxiliary file" in a separate window.
;;
;; To use this feature, set (in the database's control file) the
;; variable `forms-aux-field' to the number of a field containing
;; filenames.  Then the command `forms-aux-open-file' will open the
;; file named in that field for the current record, displaying it in a
;; separate window.
;;
;; Only one auxiliary file can be opened at a time; if there is
;; already such a file opened, then `forms-aux-open-file' will kill
;; the buffer containing it.  If the buffer is modified, all the usual
;; save prompts will apply.
;;
;; By default, `forms-aux-open-file' will not create files that do not
;; already exist.  To change this, set `forms-aux-create-file-p' to a
;; non-nil value.
;;
;; If you wish to have the auxiliary file synchronized when changing
;; records, consider using the `forms-barb' module and putting
;; `forms-aux-open-file' in the hook `forms-barb-change-record-hook'.

;;; Code:

(require 'forms)


;;;; User Options
;;;;=============
;; So forms databases can more conveniently configure them, all of
;; these are buffer-local.
(defcustom forms-aux-create-file-p nil
  "Whether `forms-aux-open-file' should create new files.

If this is non-nil, `forms-aux-open-file' will create whatever
file is named by the field numbered `forms-aux-field'.
Otherwise, it will just kill any existing auxiliary buffer and
not replace it."
  :group 'forms
  :type  'boolean)

(make-local-variable 'forms-aux-create-file-p)


;;;; Database Variables
;;;;===================
(defvar-local forms-aux-field nil
  "Field number to open as an auxiliary file.
If nil, do not open any auxiliary file.")


;;;; State Variables
;;;;================
(defvar-local forms-aux-buffer nil
  "The auxiliary file buffer for the current database.")


;;;; Implementation
;;;;===============
;;;###autoload
(defun forms-aux-open-file ()
  "Open the auxiliary file for the current record.

The filename to open is the current record's value for the field
numbered by `forms-aux-field'.  Open this file, if it exists, in
a separate window, and kill any other buffer previously created
by this function. 

Return the newly opened buffer, but do not select it.

If `forms-aux-field' is nil, do nothing."
  (interactive) 
  (when forms-aux-field
    (when forms-aux-buffer (kill-buffer forms-aux-buffer))
    (let* ((window      (selected-window))
           (filename    (nth forms-aux-field forms-fields))
           (aux-buffer  (when (or forms-aux-create-file-p
                                  (file-exists-p filename))
                         (find-file-other-window filename))))
      (select-window window) 
      (setq forms-aux-buffer aux-buffer))))

(provide 'forms-aux)
;;; forms-aux.el ends here
