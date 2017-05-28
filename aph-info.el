;;; aph-info.el --- Extensions for `info'            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: tools, lisp

;; Dependencies: `info'

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

;; Commands for use in `info-mode'.

;;; Code:

(require 'info)


;;;; Commands
;;===========
;;;###autoload
(defun aph/info-mode-or-clone-buffer (prefix)
  "Enter info mode or clone info buffer.

In an info buffer when no prefix argument has been supplied,
clone the buffer (as `clone-buffer').  Otherwise, enter info
mode (as `info')."
  (interactive "P")
  (if (and (eq major-mode 'Info-mode) (not prefix))
      (clone-buffer (not :rename) :popto)
    (setq prefix-arg prefix)
    (call-interactively #'info)))

;;;###autoload
(defun aph/Info-final-menu-item ()
  "Go to the node of the last menu item.

This command duplicates the functionality of the 0 key in the
standalone info application."
  (interactive)
  (Info-goto-node (Info-extract-menu-counting nil)))

(provide 'aph-info)
;;; aph-info.el ends here
