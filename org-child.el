;;; org-child.el --- Functional Org trees            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience, Org

;; Dependencies: `org'

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

;; This module provides some useful functions to enable more
;; "higher-level" access to the data encoded in the hierarchy of
;; headings in an Org buffer.  The goal is to abstract away the
;; nitty-gritty details (such as how to get from one heading to
;; another) and provide a simple, abstract interface.

;;; Code:

(require 'org)
(eval-when-compile (require 'dash))


;;;; Core Functional Idioms
;;=========================
(defun org-child-reduce (fn &optional acc pom)
  "Reduce FN over the immediate children of heading at POM.

The function FN should take a single argument (the accumulator);
it will be called once for each child heading, with point on that
heading, and is expected to extract whatever information it needs
from the entry using functions like `org-entry-get'.

The optional argument ACC specifies the initial value of the
accumulator; if it is omitted, it defaults to nil.

If the optional argument POM is omitted, it defaults to point.

If the heading at POM has no children, ACC is returned and FN is
not called.

If POM is before the first heading in the buffer, FN is reduced
over the top-level headings.

The final value of the accumulator is returned, and point is
restored to its initial position."
  (setq pom (or pom (point)))
  (save-excursion
    (goto-char pom) 
    (when (if (org-before-first-heading-p)
	      (progn (org-next-visible-heading 1) (org-at-heading-p))
	    (org-goto-first-child))
      (while (progn
	       (setq acc (funcall fn acc))
	       (org-get-next-sibling))))
    acc))

(defun org-child-map (fn &optional pom)
  "Map FN over the immediate children of heading at POM.

The function FN should take no arguments; it will be called once
for each child heading, with point on that heading, and is
expected to extract whatever information it needs from the entry
using functions like `org-entry-get'.

If POM is omitted, it defaults to point.

If POM is before the first heading in the buffer, FN is mapped
over the top-level headings.

The return value is a list of all values returned by FN, and
point is restored to its initial position."
  (-> (lambda (acc)
	(cons (funcall fn) acc))
      (org-child-reduce nil pom)
      nreverse))


;;;; Introspection
;;================
(defun org-child-count (&optional pom)
  "Return the number of children for Org mode heading at POM.

If POM (a number or a marker) is omitted, use point.  If POM is
before the first heading of the buffer, count the top-level
headings." 
  (org-child-reduce #'1+ 0 pom))

(defun org-child-get-property (pom prop &optional inherit literal-nil)
  "Return list of PROP values for all children of heading at POM.

If POM is before the first heading of the buffer, collect
property values from all top-level headings instead.

See `org-entry-get' for use of optional parameters."
  (-> (lambda ()
	(org-entry-get (point) prop inherit literal-nil))
      (org-child-map pom)))

(defun org-child-sum-property (pom prop &optional inherit default)
  "Return sum of PROP values for all children of heading at POM.

If INHERIT is non-nil, use inherited values for PROP when
appropriate.

If DEFAULT is non-nil, it should be a number, which will be used
in place of missing values for PROP (after inheritance, if
applicable); if it is omitted, 0 is used.  In any case, 0 will be
used for non-numeric values.

If POM is before the first heading of the buffer, sum property
values from the top-level headings instead."
  (setq default (or default 0))
  (-> (lambda (acc)
	(let* ((raw-val  (org-entry-get (point) prop inherit))
	       (val      (if raw-val (string-to-number raw-val) default))) 
	  (+ acc val)))
      (org-child-reduce 0 pom)))


;;;; Movement
;;===========
;;;###autoload
(defun org-child-goto (n)
  "Goto the Nth child of heading at point.

Children are counted from 1.  If heading does not have N
children, return nil and do not move point; otherwise, return
point.

If N is zero, call `org-back-to-heading' and return point.

If N is negative, goto the (-N)th child from the end (so
\(org-child-goto -1) moves to the last child)."
  (interactive "p")
  (cond
   ((zerop n)
    (org-back-to-heading)
    (point))
   
   ((< n 0)
    (let ((count (org-child-count)))
      (unless (> (- n) count)
	(org-child-goto (+ count n 1)))))
   
   ((> n 0)
    (let ((target
	   (catch 'success
	     (org-child-reduce 
	      (lambda (i)
		(if (= i n)
		    (throw 'success (point))
		  (1+ i)))
	      1)
	     nil)))
      (when target (goto-char target))))))

(provide 'org-child)
;;; org-child.el ends here
