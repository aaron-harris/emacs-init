;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CUSTOM AGENDA COMMANDS
;;;;============================================================================

(require 'aph-comparators)                ; For `aph/random-comparator'
(require 'aph-org-agenda)                 ; Skip functions and comparators


;;; Block Definitions
;;;==================
(defvar aph/org-agenda-block-calendar 
  '(agenda
    ""
    ((org-agenda-overriding-header "Calendar")
     (org-agenda-ndays 1)
     (org-agenda-sorting-strategy '(time-up category-up))
     (org-habit-show-habits nil)
     (org-agenda-use-time-grid nil)
     (org-agenda-skip-function
      '(aph/org-agenda-skip-without-match "+calendar"))))
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
`aph/org-habit-show-graphs-everywhere'."
  `(tags-todo
    ,match
    ((org-agenda-overriding-header ,header)
     (aph/org-habit-show-graphs-everywhere t)
     (org-agenda-todo-ignore-with-date nil)
     (org-agenda-todo-ignore-timestamp 'future)
     (org-agenda-todo-ignore-scheduled 'future) 
     (org-agenda-sorting-strategy '(time-up priority-down category-up))
     (org-agenda-skip-function
      '(aph/org-agenda-skip-entry-unless
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
    ((org-agenda-overriding-header ,header)
     (org-agenda-cmp-user-defined #'aph/random-comparator)
     (org-agenda-sorting-strategy '(user-defined-up)) 
     ,@(when limit `((org-agenda-max-entries ,limit))))))


;;; Custom Agenda Commands
;;;=======================
(setq org-agenda-custom-commands nil)

(add-to-list 'org-agenda-custom-commands
             '("d" . "Daily agendas"))

(add-to-list 'org-agenda-custom-commands
             `("dm" "Morning Agenda"
               (,aph/org-agenda-block-calendar
                ,(aph/org-agenda-block-scheduled-tasks
                  "Morning Tasks" "+morning|all/TODO|ACTIVE")
                ,(aph/org-agenda-block-random-tasks
                  "Breakfast Reading" "+text/MEDIA" 5))))

(add-to-list 'org-agenda-custom-commands
             `("dw" "Work Agenda"
               (,aph/org-agenda-block-calendar
                ,(aph/org-agenda-block-scheduled-tasks
                  "Work Tasks" "+work|all/TODO|ACTIVE")
                ,(aph/org-agenda-block-random-tasks
                  "Listening" "+audio")
                (tags
                 "+work+LEVEL=1+TODO=\"\""
                 ((org-agenda-overriding-header "Work Notes:"))))))

(add-to-list 'org-agenda-custom-commands
             `("de" "Evening Agenda"
               (,aph/org-agenda-block-calendar
                ,(aph/org-agenda-block-scheduled-tasks
                  "Evening Tasks" "+evening|all/TODO|ACTIVE")
                ,(aph/org-agenda-block-random-tasks
                  "Dinner Entertainment" "+video/MEDIA" 5)
                ,(aph/org-agenda-block-random-tasks
                  "Leisure" "+leisure/TODO" 5))))

(add-to-list 'org-agenda-custom-commands
             `("ds" "Weekend Agenda"
               (,aph/org-agenda-block-calendar
                ,(aph/org-agenda-block-scheduled-tasks
                  "Weekend Tasks" "+weekend|all/TODO|ACTIVE")
                ,(aph/org-agenda-block-random-tasks
                  "Leisure" "+leisure/TODO|MEDIA" 5))))

(add-to-list 'org-agenda-custom-commands
             `("c" "Computer Agenda"
               (,(aph/org-agenda-block-scheduled-tasks
                  "Scheduled Tasks" "+computer-review/TODO|ACTIVE")
                ,(aph/org-agenda-block-random-tasks
                  "Random Tasks" "+computer/TODO" 10))))

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


;;; Smart Agenda Setup
;;;===================
(setq aph/org-agenda-smart-agenda-views '("dm" "dw" "de" "ds")
      aph/org-agenda-workday            '(10 . 18.5))

(provide 'init-org-agenda)
