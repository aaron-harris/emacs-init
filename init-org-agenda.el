;;; init-org-agenda.el --- Personal Emacs config (Org agendas) -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016  Aaron Harris
;; Author: Aaron Harris <meerwolf@gmail.com>

;;; Code:

(require 'org-agenda-count)
(require 'org-agenda-skip)
(require 'org-compare)
(require 'org-habit-everywhere)
(require 'org-match)


;;;; Block Definitions
;;====================
(defvar aph/org-agenda-block-calendar
  '(agenda
    ""
    ((org-agenda-overriding-header "Calendar")
     (org-agenda-ndays 1)
     (org-agenda-sorting-strategy '(time-up category-up))
     (org-habit-show-habits nil)
     (org-agenda-use-time-grid nil)
     (org-agenda-skip-function
      '(org-match-skip "+calendar" :keep))))
  "Custom agenda block containing an overview of the day.
This block (for use in `org-agenda-custom-commands') contains
those tasks tagged with the \"calendar\" tag.  Note that it has
nothing to do with the Emacs calendar.")

(defun aph/org-agenda-block-scheduled-tasks (header match)
  "Return an agenda block for scheduled tasks matching MATCH.

The returned block (a list form) defines a todo-type agenda block
with header HEADER.  This block will contain only todo items that
match MATCH and satisfy at least one of the following criteria:
 - Has an active timestamp that isn't in the future.
 - Is scheduled, again not in the future.
 - Has a deadline.
 - Has the \"ACTIVE\" todo keyword.

This block will show habit graphs, overriding the global value of
`org-habit-everywhere-p'." 
  `(tags-todo
    ,match
    ((org-agenda-overriding-header ,header)
     (org-habit-everywhere-p t)
     (org-agenda-todo-ignore-with-date nil)
     (org-agenda-todo-ignore-timestamp 'future)
     (org-agenda-todo-ignore-scheduled 'future)
     (org-agenda-sorting-strategy '(time-up priority-down category-up))
     (org-agenda-skip-function
      '(org-agenda-skip-entry-unless
        'scheduled 'deadline 'timestamp 'todo '("ACTIVE"))))))

(defun aph/org-agenda-block-random-tasks (header match &optional limit)
  "Return an agenda block for random tasks matching MATCH.

The returned block (a list form) defines a todo-type agenda block
with header HEADER that includes only todo items matching MATCH,
ordered randomly.

If LIMIT is supplied, it should be an integer, and only that many
tasks will be displayed."
  `(tags-todo
    ,match
    ((org-agenda-overriding-header
      (format "%s [%s]" ,header (org-agenda-count ,match)))
     (org-agenda-cmp-user-defined   ',(org-compare-randomly))
     (org-compare-random-refresh    t)
     (org-agenda-sorting-strategy   '(user-defined-up))
     ,@(when limit `((org-agenda-max-entries ,limit))))))


;;;; Custom Agenda Commands
;;=========================
(setq org-agenda-custom-commands nil)

(add-to-list 'org-agenda-custom-commands
             '("d" . "Daily agendas"))

(add-to-list 'org-agenda-custom-commands
             `("dm" "Morning Agenda"
               (,aph/org-agenda-block-calendar
                ,(aph/org-agenda-block-scheduled-tasks
                  "Morning Tasks" "+morning|all/TODO|ACTIVE"))))

(add-to-list 'org-agenda-custom-commands
             `("dw" "Work Agenda"
               (,aph/org-agenda-block-calendar
                ,(aph/org-agenda-block-scheduled-tasks
                  "Work Tasks" "+work|all/TODO|ACTIVE")
                (tags
                 "+work+LEVEL=1+TODO=\"\""
                 ((org-agenda-overriding-header "Work Notes:"))))))

(add-to-list 'org-agenda-custom-commands
             `("de" "Evening Agenda"
               (,aph/org-agenda-block-calendar
                ,(aph/org-agenda-block-scheduled-tasks
                  "Evening Tasks" "+evening|all/TODO|ACTIVE"))))

(add-to-list 'org-agenda-custom-commands
             `("ds" "Weekend Agenda"
               (,aph/org-agenda-block-calendar
                ,(aph/org-agenda-block-scheduled-tasks
                  "Weekend Tasks" "+weekend|all/TODO|ACTIVE"))))

(add-to-list 'org-agenda-custom-commands
             `("c" "Computer Agenda"
               (,(aph/org-agenda-block-scheduled-tasks
                  "Scheduled Tasks" "+computer-review/TODO|ACTIVE")
                ,(aph/org-agenda-block-random-tasks
                  "Random Tasks" "+computer/TODO" 10)
                ,(aph/org-agenda-block-random-tasks
                  "Reading Material" "+computer/MEDIA" 5))))

(add-to-list 'org-agenda-custom-commands
             `("e" "Emacs Agenda"
               (,(aph/org-agenda-block-scheduled-tasks
                  "Scheduled Tasks" "+CATEGORY=\"Emacs\"/TODO|ACTIVE")
                ,(aph/org-agenda-block-random-tasks
                  "Random Tasks" "+CATEGORY=\"Emacs\"/TODO" 10))))

(add-to-list 'org-agenda-custom-commands
             `("r" "Review Agenda"
               (,(aph/org-agenda-block-scheduled-tasks
                  "Scheduled for Review" "+review/TODO|ACTIVE"))))

(setq org-agenda-custom-commands (nreverse org-agenda-custom-commands))

(provide 'init-org-agenda)
;; init-org-agenda.el ends here
