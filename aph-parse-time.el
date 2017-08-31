;;; aph-parse-time.el --- Time parsing extensions    -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Aaron Harris

;; Author: Aaron Harris <aaron.patrick.harris@gmail.com>
;; Keywords: util

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

;; Extensions for the `parse-time' module built into Emacs.

;;; Code:

(eval-when-compile (require 'cl-lib))


;;;###autoload
(defun aph/parse-time-string-and-format (format-string time &optional zone)
  "As `format-time-string', but TIME is as `parse-time-string'.

Conceptually, this function is the composition

    (format-time-string FORMAT-STRING (parse-time-string TIME) ZONE)

except the exact time format returned by `parse-time-string' is
not that expected by `format-time-string', so some additional
work is necessary."
  (cl-destructuring-bind (sec min hour day month year dow dst tz)
      (parse-time-string time)
    (format-time-string
     format-string
     (encode-time (or sec 0) (or min 0) (or hour 0)
                  (or day 0) (or month 0) (or year 0) tz)
     zone)))

(provide 'aph-parse-time)
;;; aph-parse-time.el ends here
