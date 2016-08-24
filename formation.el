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
;; The basic functions are `formation-reduce' and `formation-map'.
;; See each function's docstring for more information.
;;
;; Special consideration is made for the `forms-narrow' module.  This
;; is not required, but if the database has been narrowed, then only
;; records satisfying the current narrowing predicate will be
;; considered.  If you wish to ignore any narrowing that might be in
;; effect, you should use the alternate functions
;; `formation-reduce-all' and `formation-map-all'.

;;; Code:

(require 'forms-barb) 


;;;; `formation-reduce'
;;=====================
(defun formation--filterize (fun filter)
  "Make FUN only consider records satisfying FILTER.
For use with `formation-reduce'."
  (lambda (acc)
    (if (funcall filter)
        (funcall fun acc)
      acc)))

(defun formation-reduce-all (fun &optional acc filter)
  "As `formation-reduce', but ignore narrowing."
  (setq filter  (or filter (lambda () t))
        fun     (formation--filterize fun filter))
  (let ((rec forms--current-record))
    (unwind-protect
        (let ((forms-barb-change-record-hook nil)) 
          (forms-first-record)
          (while (< forms--current-record forms--total-records)
            (setq acc (funcall fun acc))
            (forms-next-record 1))
          (setq acc (funcall fun acc)))
      (forms-jump-record rec))))

(defun formation--narrow-filter (filter)
  "Make FILTER respect `forms-narrow-mode'.

A a special case, a nil argument is considered to stand in for
the function that always returns t."
  (setq filter (or filter (lambda () t)))
  (if (bound-and-true-p forms-narrow-mode)
      (lambda () (and (funcall filter) (funcall forms-narrow--predicate)))
    filter))

(defun formation-reduce (fun &optional acc filter)
  "Reduce FUN over all visible records in current database.
Use ACC as the initial value for the accumulator, or nil if ACC
is not provided.

The function FUN should take a single argument (the accumulator)
and return a value of the same type.  The data in the current
record can be accessed using dynamic variables such as
`forms-fields'.

Do not change current record (or, more accurately, save current
record and restore it after completion).

If the database is narrowed with the `forms-narrow' module, FUN
is only called at records satisfying the current narrowing
predicate.  If you wish to ignore narrowing, see
`formation-reduce-all'.

With optional parameter FILTER (a function of no arguments), only
records for which FILTER returns non-nil are considered.  If
narrowing is also in place, both constraints apply (with FILTER
being checked first).

Run `forms-barb-change-record-hook' only once, when restoring the
initial record.  Since this is frequently necessary to refresh
some aspect of the buffer's appearance, it is run even in the
event of an error or nonlocal exit." 
  (formation-reduce-all fun acc (formation--narrow-filter filter)))


;;;; `formation-map'
;;==================
(defun formation-map-all (fun &optional filter)
  "As `formation-map', but ignore narrowing." 
  (reverse (formation-reduce-all
            (lambda (acc)
              (cons (funcall fun) acc))
            nil filter)))

(defun formation-map (fun &optional filter)
  "Call FUN once for each record, and accumulate the results.

Visit each record in the current `forms-mode' database in turn,
and call FUN with no arguments.  After all records have been
visited, return a list containing all values returned by FUN (in
the same order they were generated).

Since FUN is not called with arguments, it is expected to access
the data in the current record via dynamic variables such as
`forms-fields'.

With optional argument FILTER (another function of no arguments),
only include records for which FILTER returns non-nil.  If the
database is narrowed with the `forms-narrow' module, only records
satisfying the current narrowing predicate are included, even if
FILTER is nil.  To ignore narrowing, use `formation-map-all'.

Do not change current record (or, more accurately, save current
record and restore it after completion).

Run `forms-barb-change-record-hook' only once, when restoring the
initial record.  Since this is frequently necessary to refresh
some aspect of the buffer's appearance, it is run even in the
event of an error or nonlocal exit." 
  (formation-map-all fun (formation--narrow-filter filter)))

(provide 'formation)
;;; formation.el ends here
