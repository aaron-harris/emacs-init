;;; proctor-org.el --- ERT support for Org mode      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: tools, lisp

;; Dependencies: `proctor', `bfw', `org-capture'

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

;; This module contains macros and functions designed for testing
;; commands for use in and relating to `org-mode'.

;;; Code:

(require 'proctor)

(require 'bfw)
(require 'org-capture)


;;;; Agenda
;;=========
(defcustom proctor-org-temp-agenda-file
  "temp.org"
  "Filename that `proctor-org-with-agenda-items' should use.

If the filename is relative, it will be placed inside
`proctor-directory'."
  :group 'proctor
  :type 'string)

(defun proctor-org-list-agendas ()
  "Return a list of all agenda buffers currently open.

By design, an agenda buffer that has been protected with
`ert-with-buffer-renamed' (or `proctor-with-buffer-renamed',
etc.) is not included in this list (although
`ert-with-buffer-renamed' does create a different buffer with the
original name, and that buffer is included)."
  (bfw-get-buffers-by-regexp "^[*]Org Agenda.*[*]$"))

(defmacro proctor-org-with-agenda-items (items &rest body)
  "Evaluate BODY with temporary `org-mode' agenda ITEMS.

Create a temporary agenda file (`proctor-org-temp-agenda-file')
containing ITEMS, to be used in place of `org-agenda-files'.
Then evaluate BODY, and delete the agenda file afterwards.  Also
kill any agenda buffers that were created inside BODY.

ITEMS should be a string or a list of strings; in the latter
case, the elements are concatenated, separated by newlines.

All %-escapes that are valid in capture templates are then
expanded, although many of those don't make sense outside of the
capture process; at the present time, the only escapes that have
been tested and are known to work as expected are the timestamp
escapes (%t, %T, %u, and %U).

Care is taken to insulate the BODY environment from the user's
default Org setup.  In particular, all currently open agendas are
preserved using `proctor-with-buffers-renamed'."
  (declare (indent 1)
           (debug ([&or stringp (&rest stringp)] &body)))
  (when (listp items)
    (setq items (mapconcat #'identity items "\n")))
  `(let ((org-agenda-files
          (list (expand-file-name ,proctor-org-temp-agenda-file
                                  proctor-directory)))
         org-capture-plist)
     (proctor-with-file ,proctor-org-temp-agenda-file
         (org-capture-fill-template ,items)
       (proctor-with-buffers-renamed
           (mapcar #'buffer-name (proctor-org-list-agendas))
         (unwind-protect (progn ,@body)
           (mapcar #'bfw-kill-buffer-if-any (proctor-org-list-agendas)))))))

(provide 'proctor-org)
;;; proctor-org.el ends here
