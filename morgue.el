;;; morgue.el --- In-place kill ring processing      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience kill-ring

;; Dependencies: `dash', `s'

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

;; This module contains functions for editing the contents of the kill
;; ring (specifically, the most recent kill) in-place; that is,
;; without yanking it into a buffer first.
;;
;; To use this code, just require the module, then call `morgue-apply'
;; with suitable transforms (functions taking a string to another
;; string).  This will apply those transforms in sequence and put the
;; result onto the kill ring (without disturbing the original).
;;
;; Alternatively, you could use `morgue-yank', which is just like
;; `morgue-apply' except it yanks the result, too.
;;
;; The remaining functions define transforms usable with either of
;; these functions.  A brief list follows.
;;
;; - `morgue-map' is a higher-level transform that applies its
;;   argument to each chunk of a kill; the chunks can be specified
;;   using regexps.  This function is also useful for changing the
;;   delimiters within a string.  For instance, to change all newlines
;;   to tabs, you could use this transform:
;;
;;     (morgue-map #'identity "\n" "\t")
;;
;; - `morgue-zap' ignores the part of the kill preceding the given
;;   string and returns the rest.

;;; Code:

(require 'dash)
(require 's)


;;; Applicators
;;;============
(defun morgue-apply (&rest transforms)
  "Apply TRANSFORMS to most recent kill.

Each TRANSFORM should be a function taking a single string and
producing a new string.  The composition of the TRANSFORMS (in
left-to-right order) is applied to the most recent kill in the
kill ring, and the result is placed onto the kill ring.  The
original kill is not removed.

Return the new kill."
  (let ((kill  (current-kill 0)))
    (while transforms
      (setq kill (funcall (pop transforms) kill)))
    (kill-new kill)))

(defun morgue-yank (&rest transforms)
  "As `morgue-apply', and yank the result at point."
  (let ((result  (apply #'morgue-apply transforms)))
    (insert-for-yank result)
    result))


;;; Transforms
;;;===========
(defun morgue-map (transform &optional old-seps new-sep)
  "Return function applying TRANSFORM to each part of a string.

TRANSFORM should be a function taking a single string and
producing a new string.  The function returned takes in a string,
splits it on OLD-SEPS (a regexp), applies TRANSFORM to each
substring, then joins these together with NEW-SEP as a separator,
returning the result.

If either OLD-SEPS or NEW-SEP is nil or omitted, that paramater
defaults to \"\\n\", so that TRANSFORM is applied to individual
lines."
  (let ((old-seps  (or old-seps "\n"))
        (new-sep   (or new-sep  "\n")))
    (lambda (s)
      (mapconcat transform
                 (split-string s old-seps)
                 new-sep)))) 

(defun morgue-zap (sep)
  "Return function that ignores prefix of string delimited by SEP.

The returned function takes a string and returns the substring
starting after the first instance of SEP (a string) and
continuing to the end."
  (lambda (s)
    (->> (split-string s sep)
         cdr
         (s-join sep))))


(provide 'morgue)
;;; morgue.el ends here
