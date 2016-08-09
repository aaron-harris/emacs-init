;;; forms-skip.el --- Skip some records in `forms-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: data, forms

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

;; This module implements a form of narrowing for `forms-mode'
;; databases.  When narrowing is in effect, some records are "hidden",
;; so that navigation commands will skip over them.
;;
;; This module is a work in progress, so at present there is no
;; interactive way to apply the narrowing effect.  You can, however,
;; supply an elisp predicate to `forms-skip-predicate', and records
;; "satisfying" that predicate will be skipped.  (Note that this use
;; of "satisfying" may be misleading; the function is called with no
;; arguments, and we consider a record to satisfy the predicate if it
;; returns non-nil when called with that record current.)
;;
;; In order to implement the skipping, note that this module defines
;; new navigation commands and remaps the basic `forms-mode' commands
;; to use these.  These are:
;;
;;   `forms-next-record' => `forms-skip-forward'
;;   `forms-prev-record' => `forms-skip-backward'
;;
;; Note that this interface is still incomplete, and using commands
;; other than those already implemented will ignore the narrowing
;; effect.

;;; Code:

(require 'forms)


;;;; State Variables
;;==================
(defvar-local forms-skip-predicate nil
  "Predicate determining which records to skip.

This predicate is called with no arguments whenever a new record
is displayed.  If it returns non-nil, that record is skipped.

If this variable is nil, all records are visible.")


;;;; Navigation Commands
;;======================
(defun forms-skip-forward (arg)
  "As `forms-next-record', but obey `forms-skip-predicate'.

If all records after the current one satisfy
`forms-skip-predicate', then signal an error and stay on this
record."
  (interactive "p")
  (if (null forms-skip-predicate)
      (forms-next-record arg)
    (let ((stepper    (if (< arg 0) #'forms-prev-record #'forms-next-record))
          (saved-rec  forms--current-record))
      (condition-case err
          (dotimes (i (abs arg))
            (while (progn (funcall stepper 1)
                          (not (funcall forms-skip-predicate))))
            (setq saved-rec forms--current-record))
        (error (forms-jump-record saved-rec)
               (signal (car err) (cdr err)))))))

(defun forms-skip-backward (arg)
  "As `forms-prev-record', but obey `forms-skip-predicate'."
  (interactive "p")
  (forms-skip-forward (- arg)))


;;;; Remapping
;;============
(defun forms-skip--remap ()
  "Remap commands for `forms-skip' module."
  (dolist (pair
           '((forms-next-record . forms-skip-forward)
             (forms-prev-record . forms-skip-backward)))
    (define-key forms-mode-edit-map `[remap ,(car pair)] (cdr pair))))

(add-hook 'forms-mode-hook #'forms-skip--remap)

(provide 'forms-skip)
;;; forms-skip.el ends here
