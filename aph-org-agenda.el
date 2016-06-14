;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG MODE AGENDA SUBROUTINES
;;;;============================================================================

;;; This file contains functions which are useful in building custom
;;; agenda commands.

;; Some functions in this file require the 'aph-comparators library at runtime.


;;; Basic Extensions
;;;=================
(defun aph/org-agenda-redo ()
  "As `org-agenda-redo' with prefix arg.

This is exactly the command bound by default to g in
`org-agenda-mode', except it's not a lambda."
  (interactive)
  (org-agenda-redo t))


;;; Habits
;;;=======
(defvar aph/org-habit-show-graphs-everywhere nil
  "If non-nil, show habit graphs in all types of agenda buffers.

Normally, habits display consistency graphs only in
\"agenda\"-type agenda buffers, not in other types of agenda
buffers.  Set this variable to any non-nil value to show
consistency graphs in all Org mode agendas.")

(defun aph/org-agenda-mark-habits ()
  "Mark all habits in current agenda for graph display.

This function enforces `aph/org-habit-show-graphs-everywhere' by
marking all habits in the current agenda as such.  When run just
before `org-agenda-finalize' (such as by advice; unfortunately,
`org-agenda-finalize-hook' is run too late), this has the effect
of displaying consistency graphs for these habits.

When `aph/org-habit-show-graphs-everywhere' is nil, this function
has no effect."
  (when (and aph/org-habit-show-graphs-everywhere
         (not (get-text-property (point) 'org-series)))
    (let ((cursor (point))
          item data) 
      (while (setq cursor (next-single-property-change cursor 'org-marker))
        (setq item (get-text-property cursor 'org-marker))
        (when (and item (org-is-habit-p item)) 
          (with-current-buffer (marker-buffer item)
            (setq data (org-habit-parse-todo item))) 
          (put-text-property cursor
                             (next-single-property-change cursor 'org-marker)
                             'org-habit-p data))))))

(advice-add #'org-agenda-finalize :before #'aph/org-agenda-mark-habits)

(provide 'aph-org-agenda)
