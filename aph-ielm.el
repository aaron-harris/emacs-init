;;; aph-ielm.el --- Extensions for `ielm'            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: tools, lisp

;; Dependencies: `ielm'

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

;; Commands for use with `ielm'.

;;; Code:

(require 'ielm)


;;;; Subroutines
;;==============
(defun aph/ielm-get-last-output (&optional arg)
  "Return the last output produced by `ielm'.

With argument N > 0, instead return the Nth last output.
With argument N < 0, return the Nth output since last clear.
With argument N = 0, do nothing and return nil.

If N greater in absolute value than the number of uncleared
outputs in the ielm buffer, return nil."
  (let* ((arg              (or arg 1)) 
         (output-regexp    (concat "^" ielm-prompt ".+\n"
                                   "\\(?:\s.+\n\\)*"
                                   "\\([^\s].+\\)"))
        (search-function  (if (< arg 0)
                              #'search-forward-regexp
                            #'search-backward-regexp)))
    (unless (zerop arg)
      (with-current-buffer "*ielm*"
        (save-excursion 
          (goto-char (if (> arg 0) (point-max) (point-min)))
          (if (funcall search-function output-regexp nil :noerror (abs arg))
              (match-string 1)
            nil))))))


;;;; Commands
;;===========
;;;###autoload
(defun aph/ielm-copy-last-output (&optional arg)
  "Copy the last output produced by `ielm' to the kill ring.

With argument N > 0, instead copy the Nth last output.
With argument N < 0, copy the Nth output since last clear.
With argument N = 0, do nothing.

Return the newly copied string, or nil if nothing was
copied (e.g., if the argument is greater than the number of
uncleared outputs)."
  (interactive "p")
  (kill-new (aph/ielm-get-last-output arg)))

(provide 'aph-ielm)
;;; aph-ielm.el ends here
