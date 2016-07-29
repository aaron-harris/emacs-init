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
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'vizier))


;;;; Narrowing
;;============
(defun aph/org-narrow-to-entry ()
  "Narrow buffer to current entry.

An entry is the text between a heading and the start of its first
child."
  (interactive)
  (save-excursion
    (outline-show-subtree)
    (narrow-to-region
     (progn (org-back-to-heading)        (point))
     (progn (org-next-visible-heading 1) (point)))))


;;;; Timestamps
;;=============
(defun aph/org-timestamp-date-only (timestamp)
  "Remove time info from TIMESTAMP and return new timestamp.
TIMESTAMP is not modified."
  (replace-regexp-in-string " ?[0-9][0-9]:[0-9][0-9]" "" timestamp))

(defun aph/org-find-timestamp (&optional pos type date)
  "Move point to timestamp in entry at POS, and return point. 
If no timestamp is found, return nil and do not move point.

POS defaults to point if omitted.

The TYPE parameter has the same meaning as in `org-re-timestamp'
and restricts which timestamps are considered.  The
first (earliest appearing) allowable timestamp is used.

If DATE is supplied (as a timestamp), only timestamps for that
date are considered." 
  (let ((target
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (or pos (point)))
             (org-back-to-heading)
             (aph/org-narrow-to-entry) 
             (let ((regexp (org-re-timestamp type)))
               (catch 'found
                 (while (re-search-forward regexp nil :noerror)
                   (when (or (not date)
                             (org-time=
                              (aph/org-timestamp-date-only (match-string 1))
                              (aph/org-timestamp-date-only date)))
                     (throw 'found (point))))))))))
    (when target (goto-char target))))

(defun aph/org-relative-timestamp (&optional days hours inactive)
  "Return a properly-formatted Org timestamp relative to today.

The timestamp differs from the current time by DAYS and HOURS;
positive values are in the future.  The hours are only included
in the timestamp if HOURS is supplied.  (To get the current
timestamp, including the hour, supply 0 for HOURS.)

The timestamp is active unless INACTIVE is non-nil."
  (let* ((format  (funcall (if hours #'cdr #'car) org-time-stamp-formats))
         (time    (current-time))
         (hours   (+ (or hours 0) (* 24 (or days 0)))))
    (cl-incf (nth 1 time) (* 60 60 hours))
    (when inactive
      (setq format (concat "[" (substring format 1 -1) "]")))
    (format-time-string format time)))


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
    (require 'aph-org-table)
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


;;;; Capture
;;==========
(defun aph/org-capture-add-properties (template &optional props)
  "Append a property drawer containing PROPS to the capture TEMPLATE.

PROPS is an alist associating property names (strings) to their
desired values (also strings, which will typically include
template escapes like '%^').

If PROPS is omitted, the property drawer will be empty.

Note that Org syntax currently requires that the property drawer
come immediately after the headline in any entry, and this
function makes no attempt to guarantee that, so TEMPLATE should
not contain any line breaks."
  (concat template "\n"
          ":PROPERTIES:\n"
          (mapconcat
           (lambda (x) (concat ":" (car x) ": " (cdr x)))
           props "\n")
          "\n:END:"))

;; This function needs to be a cl-defun because we need to distinguish between
;; the case where new-nodes is omitted and the case where it is supplied as nil.
(cl-defun aph/org-capture-choose-target
    (&optional (prompt    "Capture at")
               (new-nodes org-refile-allow-creating-parent-nodes))
  "Prompt for a location in an Org-Mode file, then jump there.

This function is intended for use with the 'function option for
capture templates.  If PROMPT is not supplied, it defaults to
\"Capture at\".

The optional parameter NEW-NODES will override the variable
`org-refile-allow-creating-parent-nodes' for the duration of this
command.  If it is omitted, the default value of the variable
will be used."
  (let* ((target
          (save-excursion
            (org-refile-get-location prompt nil new-nodes :no-exclude)))
         (file (nth 1 target))
         (pos  (nth 3 target)))
    (find-file file)
    (goto-char (or pos (point-max)))
    (org-end-of-subtree)
    (org-return)))


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
