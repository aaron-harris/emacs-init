;;; formation.el --- Functional `forms-mode`         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: data forms

;; Dependencies: `forms-barb'

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

;; This module contains functions that implement common idioms of
;; functional programming on `forms-mode'.  Note that these idioms are
;; interpreted somewhat liberally; in particular, there is no attempt
;; made to avoid side effects or other non-functional behavior.
;;
;; Functions included are as follows.  See each function's docstring
;; for more information.
;;
;; * `formation-map'
;; * `formation-reduce'

;;; Code:

(require 'forms-barb)

(defun formation-reduce (fun &optional acc)
  "Reduce FUN over all records in current database.
Use ACC as the initial value for the accumulator, or nil if ACC
is not provided.

The function FUN should take a single argument (the accumulator)
and return a value of the same type.  The data in the current
record can be accessed using dynamic variables such as
`forms-fields'.

Do not change current record (or, more accurately, save current
record and restore it after completion).

Run `forms-barb-change-record-hook' only once, when restoring the
initial record.  Since this is frequently necessary to refresh
some aspect of the buffer's appearance, it is run even in the
event of an error or nonlocal exit."
  (save-excursion
    (unwind-protect
        (let ((forms-barb-change-record-hook nil)) 
          (forms-first-record)
          (while (< forms--current-record forms--total-records)
            (setq acc (funcall fun acc))
            (forms-next-record 1))
          (setq acc (funcall fun acc)))
      (run-hooks 'forms-barb-change-record-hook))))

(defun formation-map (fun)
  "Call FUN once for each record, and accumulate the results.

Visit each record in the current `forms-mode' database in turn,
and call FUN with no arguments.  After all records have been
visited, return a list containing all values returned by FUN (in
the same order they were generated).

Since FUN is not called with arguments, it is expected to access
the data in the current record via dynamic variables such as
`forms-fields'.

Do not change current record (or, more accurately, save current
record and restore it after completion).

Run `forms-barb-change-record-hook' only once, when restoring the
initial record.  Since this is frequently necessary to refresh
some aspect of the buffer's appearance, it is run even in the
event of an error or nonlocal exit."
  (reverse (formation-reduce
            (lambda (acc)
              (cons (funcall fun) acc)))))

(provide 'formation)
;;; formation.el ends here
