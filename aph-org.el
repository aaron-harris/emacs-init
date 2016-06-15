;;; aph-org.el --- Extensions for Org mode           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: outlines, hypermedia, wp

;; Dependencies: `org'

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

;; Code extending the `org' package.

;;; Code:

(require 'org)
(eval-when-compile (require 'vizier))


;;;; Multimodal Commands
;;======================
(defun aph/org-cycle-with-smart-tab (&optional arg)
  "As `org-cycle', but fall back to `smart-tab'.

Normally, if the command `org-cycle' can find nothing to do, it
falls back to the global binding for TAB, subject to the option
`org-cycle-emulate-tab'.  This command behaves the same as
`org-cycle', except it falls back to `smart-tab' instead if
`smart-tab-mode' is enabled."
  (interactive "P")
  (vizier-with-advice-if (bound-and-true-p smart-tab-mode) 
      ;; Make `org-cycle' use `smart-tab' as fallback action.
      ((global-key-binding
	:before-until
	(lambda (keys &optional accept-default)
	  (when (equal keys "\t") #'smart-tab)))
       ;; Prevent `smart-tab' from using `org-cycle' as its fallback.
       (smart-tab-default :override #'indent-for-tab-command))
    (org-cycle arg)))


;;;; Number Twiddling
;;===================
(defun aph/org-increase-number (&optional inc)
  "As `org-increase-number-at-point', but more flexible.

As `org-increase-number-at-point', but first reposition point
within a table cell.  Specifically, when inside an Org table and
not on a number, move to the end of the cell.  This handles the
typical case where the cell contains only a right-justified
number and point is at the beginning of the cell (on a leading
space)."
  (interactive "p")
  (when (and (org-table-p)
             (not (number-at-point)))
    (aph/org-table-end-of-this-field))
  (org-increase-number-at-point inc))

(defun aph/org-decrease-number (&optional inc)
  "As `org-decrease-number-at-point', but more flexible.
See `aph/org-increase-number' for more details."
  (interactive "p")
  (aph/org-increase-number (- (or inc 1))))


;;;; Agenda
;;=========
(defun aph/org-agenda-redo ()
  "As `org-agenda-redo' with prefix arg.

This is exactly the command bound by default to g in
`org-agenda-mode', except it's not a lambda."
  (interactive)
  (org-agenda-redo t))


;;;; Refile
;;=========
;;;###autoload
(defun aph/org-goto-last-refile ()
  "Goto last Org-mode item refiled.

This has the same effect as supplying a C-u C-u prefix argument
to `org-agenda-refile'.  It is intended for use globally, where a
keybinding for that function is not appropriate."
  (interactive)
  (org-agenda-refile '(16)))

(provide 'aph-org)
;;; aph-org.el ends here
