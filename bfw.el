;;; bfw.el --- Buffer, frame, and window library     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: extensions

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

;; This module is a library containing functions associated with
;; buffers, frames, and windows.
;;
;; Included functions are as follows.  See individual function
;; docstrings for more detailed information.
;;
;; `bfw-get-nth-window-with-predicate':
;;
;;     This is a version of `get-window-with-predicate' that takes a
;;     numerical argument.
;;
;; `bfw-get-nth-window-not-dedicated':
;;
;;     This is a non-interactive version of `other-window' that skips
;;     windows dedicated to their buffers.

;;; Code:


;;;; Window Search
;;================
(defun bfw-get-nth-window-with-predicate
    (n predicate &optional minibuf all-frames default)
  "As `get-window-with-predicate', but return the Nth success.

If N is positive, return the Nth window forward in the cyclic
ordering of windows that is not dedicated to its current buffer.

If N is negative, instead return the (- N)th window backwards.

If N is zero, return selected window if it is not dedicated and
DEFAULT otherwise (or nil if DEFAULT is not supplied).

The parameters MINIBUF, ALL-FRAMES, and DEFAULT have the same
meanings as in `get-window-with-predicate'."
  (cond
   ((null (get-window-with-predicate predicate minibuf all-frames))
    default)
   ((= n 0)
    (if (funcall predicate (selected-window))
        (selected-window)
      default))
   (t
    (let*  ((cycler  (cond
                      ((> n 0) #'next-window)
                      ((< n 0) #'previous-window)))
            (cursor  (selected-window))
            (i        (abs n)))
      (while (> i 0)
        (setq cursor (funcall cycler cursor minibuf all-frames))
        (when (funcall predicate cursor)
          (setq i (1- i))))
      cursor))))

(defun bfw-get-nth-window-not-dedicated
    (n &optional minibuf all-frames default)
  "Return the Nth window forward that is not dedicated.

If N is positive, return the Nth window forward in the cyclic
ordering of windows that is not dedicated to its current buffer.

If N is negative, instead return the (- N)th window backwards.

If N is zero, return selected window if it is not dedicated and
DEFAULT otherwise (or nil if DEFAULT is not supplied).

The parameters MINIBUF, ALL-FRAMES, and DEFAULT have the same
meanings as in `get-window-with-predicate'."
  (bfw-get-nth-window-with-predicate
   n
   (lambda (win) (not (window-dedicated-p win)))
   minibuf all-frames default))

(provide 'bfw)
;;; bfw.el ends here
