;;; forms-narrow.el --- Narrowing for `forms-mode'   -*- lexical-binding: t; -*-

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
;; so that navigation commands will skip over them.  Note that this
;; functionality is distinct from the built-in narrowing functionality
;; (e.g., `narrow-to-region'), which cannot affect more than a single
;; record in `forms-mode'.
;;
;; This module is a work in progress, so at present there is no
;; interactive way to apply the narrowing effect.  You can, however,
;; supply an elisp predicate to `forms-narrow-predicate', and only
;; records "satisfying" that predicate will be shown.  (Note that this
;; use of "satisfying" may be misleading; the function is called with
;; no arguments, and we consider a record to satisfy the predicate if
;; it returns non-nil when called with that record current.)  You will
;; also need to enable `forms-narrow-mode' manually (see below).
;;
;; Replacement of the basic `forms-mode' navigation commands (e.g.,
;; `forms-next-record') is implemented via the minor mode
;; `forms-narrow-mode'.  In the future, the commands to apply and
;; remove the narrowing will handle enabling and disabling of this
;; mode, but for the time being, this mode must be enabled manually.
;; However, this mode does nothing if `forms-narrow-predicate' has not
;; been set, so it is safe to enable this mode in `forms-mode-hook'.
;;
;; The mapping from basic `forms-mode' commands to revised
;; `forms-narrow-mode' commands is as follows:
;;
;;   `forms-next-record' => `forms-narrow-next-record'
;;   `forms-prev-record' => `forms-narrow-prev-record'
;;
;; Note that this interface is still incomplete, and using commands
;; other than those already implemented will ignore the narrowing
;; effect.

;;; Code:

(require 'forms)


;;;; State Variables
;;==================
(defvar-local forms-narrow-predicate nil
  "Predicate determining which records to show.

This predicate is called with no arguments whenever a new record
is displayed.  If it returns nil, that record is skipped.

If this variable is nil, all records are visible.")


;;;; Navigation Commands
;;======================
(defun forms-narrow-next-record (arg)
  "As `forms-next-record', but obey `forms-narrow-predicate'.

If all records after the current one satisfy
`forms-narrow-predicate', then signal an error and stay on this
record."
  (interactive "p")
  (if (null forms-narrow-predicate)
      (forms-next-record arg)
    (let ((stepper    (if (< arg 0) #'forms-prev-record #'forms-next-record))
          (saved-rec  forms--current-record))
      (condition-case err
          (dotimes (i (abs arg))
            (while (progn (funcall stepper 1)
                          (not (funcall forms-narrow-predicate))))
            (setq saved-rec forms--current-record))
        (error (forms-jump-record saved-rec)
               (signal (car err) (cdr err)))))))

(defun forms-narrow-prev-record (arg)
  "As `forms-prev-record', but obey `forms-narrow-predicate'."
  (interactive "p")
  (forms-narrow-next-record (- arg)))


;;;; Minor Mode
;;=============
(defvar forms-narrow-mode-map
  (let ((keymap (make-sparse-keymap)))
    (dolist (pair
             '((forms-next-record . forms-narrow-next-record)
               (forms-prev-record . forms-narrow-prev-record)))
      (define-key keymap `[remap ,(car pair)] (cdr pair)))
    keymap)
  "Keymap for `forms-narrow-mode'.")

(define-minor-mode forms-narrow-mode
  "Minor mode for narrowed `forms-mode' databases."
  :group forms
  :lighter " Narrow"
  :keymap  'forms-narrow-mode-map
  :require 'forms-narrow)

(provide 'forms-narrow)
;;; forms-narrow.el ends here
