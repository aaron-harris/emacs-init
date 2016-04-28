;;; enumerate.el --- Number lines in the region      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

;; Dependencies: `dash'

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

;; This module contains commands that number the lines in the region
;; in various ways.  These functions are similar to, but not directly
;; extensions of, `rectangle-number-line'.
;;
;; The basic command is `enumerate-lines'.  This adds line numbers to
;; each line in the region.  With a prefix argument, it can start the
;; numbering at a number other than 1.
;;
;; There is also `enumerate-alpha'.  This adds line numbers to each
;; line in the region, but according to how they would be sorted
;; alphabetically, instead of their current position.
;;
;; Both commands are autoloaded.

;;; Code:

(require 'dash)


;;; Subroutines
;;;============
(defun enumerate--bol (&optional pos)
  "Return position of first character on line containing POS.
If POS is omitted, use position of point.

Do not move point."
  (save-excursion
    (let ((pos (or pos (point))))
      (goto-char pos)
      (line-beginning-position))))

(defun enumerate--eol (&optional pos)
  "Return position of last character on line containing POS.
If POS is omitted, use position of point.

Do not move point."
  (save-excursion
    (let ((pos (or pos (point))))
      (goto-char pos)
      (line-end-position))))


;;; Commands
;;;=========

;;;###autoload
(defun enumerate-lines (beg end &optional offset eol sep format-str)
  "Insert a number for each line in the region.
Non-interactively, act on the text between BEG and END.

If the region is not active, act on the entire buffer.

If the region to be acted on starts or ends in the middle of a
line, a line number will still be inserted for that line.

If OFFSET is supplied (interactively, as a numeric prefix
argument), it is used as a starting point for the numbering;
otherwise, it defaults to 1.

If EOL is non-nil, the numbers will be appended to the end of
each line instead of being prepended to the beginning.

The SEP argument is the string separating the line number from
the rest of the line; i.e., it will ordinarily follow the number
but precedes it if EOL is supplied.  This defaults to a single
space.

The FORMAT-STR argument can be used to configure how the
numbers are displayed.  This is a string similar to those used in
the `format' function, but all ordinary format specifiers should
be double-escaped (e.g., \"%%d\"), and the special escape \"%n\"
will be replaced with the maximum number of digits of any line
number before the other escapes are interpreted.  The default
value for FORMAT-STR is \"%%%nd\"."
  (interactive "r\np")
  (let* ((beg        (if (use-region-p) (enumerate--bol beg) (point-min)))
         (end        (if (use-region-p) (enumerate--eol end) (point-max)))
         (offset     (1- (or offset 1)))
         (sep        (or sep " "))
         (format-str (or format-str "%%%nd"))
         (regexp     (if eol "$" "^")) 
         (pad        (->> (count-lines beg end)
                          (max 1)
                          (+ offset 1)
                          (format "%d")
                          length))
         (formatter  (format-spec format-str (format-spec-make ?n pad))))
    (save-excursion
      (save-restriction
        (goto-char beg)
        (narrow-to-region beg end) 
        (while (progn
                 (if eol (end-of-line) (beginning-of-line))
                 (insert (concat (when eol sep)
                                 (format formatter
                                         (+ (line-number-at-pos) offset))
                                 (unless eol sep)))
                 (end-of-line)
                 (= (forward-line) 0)))))))

;;;###autoload
(defun enumerate-alpha (beg end &optional offset sep format-string)
  "Number lines in region according to alphabetic order.

As `enumerate-lines', except lines are numbered according to
their alphabetic order instead of their position, and the option
to put the number at the end of the line is unavailable."
  (interactive "r\np")
  (let* ((beg (if (use-region-p) (enumerate--bol beg) (point-min)))
         (end (if (use-region-p) (enumerate--eol end) (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        ;; Append ordinary line numbers to eol, sort, then add the
        ;; numbers we want to bol.
        (enumerate-lines (point-min) (point-max) 1 :eol " " "%%0%nd")
        (sort-lines nil (point-min) (point-max))
        (enumerate-lines (point-min) (point-max) offset nil sep format-string)
        ;; Move the eol line numbers to bol and sort again to recover
        ;; original order.
        (goto-char (point-min))
        (while (re-search-forward "^\\(.*\\) \\([0-9]+\\)$" nil :noerr)
          (replace-match "\\2 \\1"))
        (sort-lines nil (point-min) (point-max))
        ;; Remove extraneous numbers.
        (goto-char (point-min))
        (while (re-search-forward "^\\([0-9]+\\) " nil :noerr)
          (replace-match "")
          (end-of-line))))))

(provide 'enumerate)
;;; enumerate.el ends here
