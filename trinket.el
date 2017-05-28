;;; trinket.el --- Simple tools                      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

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

;; This module contains general-use functions that don't seem to
;; belong anywhere else.

;;; Code:

(defun trinket-bol (&optional pos)
  "Return position of first character on line containing POS.
If POS is omitted, use position of point.

Do not move point."
  (save-excursion
    (let ((pos (or pos (point))))
      (goto-char pos)
      (line-beginning-position))))

(defun trinket-eol (&optional pos)
  "Return position of last character on line containing POS.
If POS is omitted, use position of point.

Do not move point."
  (save-excursion
    (let ((pos (or pos (point))))
      (goto-char pos)
      (line-end-position))))

(provide 'trinket)
;;; trinket.el ends here
