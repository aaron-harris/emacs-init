;;; tidy.el --- Easily clean up unwanted buffers     -*- lexical-binding: t; -*-

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

;; This module contains commands that clean up the currently displayed
;; buffers in some way.

;; At the moment, only one command is implemented, `tidy-buffers'.
;; This command any unwanted buffers (e.g., help buffers, the messages
;; buffer, etc.); the intent is that these are transient buffers that
;; you don't want to keep around, so once they've served their
;; purpose, you can invoke `tidy-buffers' and get back to work.  The
;; list of buffers is configurable via the option
;; `tidy-unwanted-buffer-list'.

;;; Code:

(defgroup tidy nil
  "Easily clean up unwanted buffers."
  :prefix "tidy-"
  :link   '(emacs-commentary-link "tidy")
  :group  'convenience)

(defcustom tidy-unwanted-buffer-list
  '("*Help*"
    "*Apropos*"
    "*Messages*"
    "*Completions*" 
    "*Command History*"
    "*Compile-Log*"
    "*disabled command*")
  "List of buffer names that `tidy-buffers' should bury."
  :type '(repeat string))

;;;###autoload
(defun tidy-buffers (&optional kill frame)
  "Bury all buffers named in `tidy-unwanted-buffer-list'.

The optional parameters KILL and FRAME are just as in the
function `quit-windows-on', except FRAME defaults to t (so that
only windows on the selected frame are considered).

Note that a nil value for FRAME cannot be distinguished from an
omitted parameter and will be ignored; use some other value if
you want to quit windows on all frames." 
  (interactive)
  (let ((frame (or frame t)))
    (dolist (buffer tidy-unwanted-buffer-list)
      (ignore-errors
        (quit-windows-on buffer kill frame)))))

(provide 'tidy)
;;; tidy.el ends here
