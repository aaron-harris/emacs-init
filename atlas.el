;;; atlas.el --- In-buffer map and reduce            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: lisp

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

;; This module provides implementations of the basic tools of
;; functional programming (map and reduce) that operate over buffer
;; text.
;;
;; Functions provided are as follows:
;;
;; `atlas-map-lines':
;;
;;     Map a function f over each line in the specified region,
;;     replacing each line with its image under f.

;;; Code:

(defun atlas-map-lines (fun start end)
  "Replace each line between START and END with result of FUN.

For each line between START and END, replace it with the result
of calling FUN (a function of one argument) with the text of that
line (as a string)."
  (save-excursion
    (save-restriction
      (goto-char start)
      (narrow-to-region start end) 
      (while (not (eobp))
        (re-search-forward "^.*$")
        (let ((line (match-string 0)))
          (replace-match (save-match-data (funcall fun line))))))))

(provide 'atlas)
;;; atlas.el ends here
