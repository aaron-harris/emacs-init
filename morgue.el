;;; morgue.el --- In-place kill ring processing      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience kill-ring

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

;; 

;;; Code:

(require 'dash)
(require 's)

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

If OLD-SEPS or NEW-SEP is nil or omitted, both default to \"\n\",
so that TRANSFORM is applied to individual lines."
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
    (->> (split-string s char)
         cdr
         (s-join char))))


(provide 'morgue)
;;; morgue.el ends here
