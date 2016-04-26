;;; filet.el --- In-place kill ring processing       -*- lexical-binding: t; -*-

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

(defun filet (transform)
  "Apply TRANSFORM to most recent kill.

TRANSFORM should be a function taking a single string and
producing a new string.  It is called on the most recent kill in
the kill ring, and the result is placed onto the kill ring.  Note
that the original kill is not removed.

Return the new kill."
  (kill-new (funcall transform (current-kill 0))))


(provide 'filet)
;;; filet.el ends here
