;;; org-habit-everywhere.el --- Habits in all agendas -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: calendar

;; Dependencies: `org-barb', `org-habit'

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

;; By default, habit graphs are only shown in "agenda"-type buffers,
;; not in other types of `org-mode' agendas (e.g., todo lists).  This
;; module adds support for displaying habit graphs in any agenda type.
;;
;; To use this feature, just require this module and set the variable
;; `org-habit-everywhere-p' to t.  You can do this globally, to apply
;; to all agendas all the time, or you can set it in a particular
;; custom agenda command to apply only to that command.

;;; Code:

(require 'org-barb)

(defcustom org-habit-everywhere-p nil
  "Whether habit graphs should be displayed in all agendas."
  :group 'org-habit
  :type  'boolean)

(defun org-habit-everywhere-mark ()
  "Mark all habits in current agenda for graph display.

This function enforces `org-habit-everywhere-p' by marking all
habits in the current agenda as such.  When run in
`org-barb-agenda-pre-finalize-hook', this has the effect of
displaying consistency graphs for these habits.

When `org-habit-everywhere-p' is nil, this fucntion has no
effect."
  (require 'org-habit)
  (when (and org-habit-everywhere-p
             (not (get-text-property (point) 'org-series))) 
    (let ((cursor (point))
          item data)
      (while (setq cursor (next-single-property-change cursor 'org-marker))
        (setq item (get-text-property cursor 'org-marker))
        (when (and item (org-is-habit-p item))
          (with-current-buffer (marker-buffer item)
            (setq data (org-habit-parse-todo item)))
          (put-text-property
           cursor
           (next-single-property-change cursor 'org-marker)
           'org-habit-p data))))))

(add-hook 'org-barb-agenda-pre-finalize-hook #'org-habit-everywhere-mark)


;;;; Unloading
;;============
(defun org-habit-everywhere-unload ()
  "Undo changes made to Emacs by module `org-habit-everywhere'."
  (remove-hook 'org-barb-agenda-pre-finalize-hook #'org-habit-everywhere-mark))

(provide 'org-habit-everywhere)
;;; org-habit-everywhere.el ends here
