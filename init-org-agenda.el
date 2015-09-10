;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CUSTOM AGENDA COMMANDS
;;;;============================================================================

(require 'aph-functions)                ; For aph/random-comparator
(require 'aph-org)


;;; Block Definitions
;;;==================
;;; For ease of reuse, we define some functions to create custom agenda blocks.
(defun aph/org-agenda-block-tagged-agenda (header tag &optional only)
  "Return an agenda block for items tagged with TAG.

The returned block (a list form) has header HEADER and includes
only items tagged with :all: or TAG, but excludes habits.  If
ONLY is non-nil, it also excludes :all:-tagged items."
  `(agenda
    ""
    ((org-agenda-overriding-header ,header)
     (org-agenda-ndays 1)
     (org-agenda-sorting-strategy '(time-up category-up))
     (org-habit-show-habits nil)
     (org-agenda-use-time-grid nil)
     (org-agenda-skip-function
      ',(if only
            `(aph/org-agenda-skip-without-tag ,tag)
          `(and (aph/org-agenda-skip-without-tag ,tag)
                (aph/org-agenda-skip-without-tag "all")))))))

(defun aph/org-agenda-block-tagged-habits (header tag &optional only)
  "Return an agenda block for habits tagged with TAG.

The returned block (a list form) has header HEADER and includes
only habits tagged with :all: or TAG.  If ONLY is non-nil, it
also excludes :all:-tagged items."
  `(agenda
    ""
    ((org-agenda-overriding-header ,header)
     (org-agenda-entry-types '(:scheduled))
     (org-agenda-skip-function 
      '(or (and (aph/org-agenda-skip-without-tag ,tag)
                ,@(unless only `((aph/org-agenda-skip-without-tag "all"))))
           (org-agenda-skip-entry-if 'notregexp ":STYLE:.*habit"))))))

(defun aph/org-agenda-block-match
    (header match kwds &optional unopened limit random)
  "Return an agenda block for items matching MATCH and KWDS.

The returned block (a list form) defines a todo-type agenda block
that has header HEADER and includes only items from open projects
that match the match-string MATCH (which should not include
keyword criteria using the ‘/’ suffix) and have a keyword in
KWDS (a list of strings).

If UNOPENED is non-nil, the block will include all items, not
just those from open projects.  If LIMIT is non-nil, the block
will only show that many tasks.  If RANDOM is non-nil, the tasks
will be ordered randomly.

This function is intended to be used in other, more specialized
functions.  This is why we take MATCH and KWDS as two distinct
parameters."
  `(tags-todo
    ,(concat match "/" (mapconcat #'identity kwds "|"))
    ((org-agenda-overriding-header ,header)
     ,@(when (not unopened)
         '((org-agenda-skip-function
            '(org-agenda-skip-subtree-if 'todo '("START" "SHELVED")))))
     ,@(when limit `((org-agenda-max-entries ,limit)))
     ,@(when random
         '((org-agenda-cmp-user-defined 'aph/random-comparator)
           (org-agenda-sorting-strategy '(user-defined-up)))))))

(defun aph/org-agenda-block-match-tasks (header match &optional limit random)
  "Return an agenda block for tasks matching MATCH.

The returned block (a list form) defines a todo-type agenda block
that has header HEADER and includes only tasks that match the
match-string MATCH. If LIMIT is non-nil, the block will only show
that many tasks.  If RANDOM is non-nil, the tasks will be ordered
randomly.

Tasks are generally items tagged with the ‘TODO’ keyword or the
‘START’ keyword that are not subtasks of an unopened project (an
item with either the ‘START’ keyword or the ‘SHELVED’ keyword).

Note that, since we are selecting only tasks, MATCH should not
include keyword criteria using the ‘/’ suffix."
  (aph/org-agenda-block-match header match
                              '("TODO" "START" "SHELVED")
                              (not :unopened) limit random))

(defun aph/org-agenda-block-match-media (header match &optional limit random)
  "Return an agenda block for tasks and media matching MATCH.

The returned block (a list form) defines a todo-type agenda block
that has header HEADER and includes only tasks and media items
that match the match-string MATCH. If LIMIT is non-nil, the block
will only show that many tasks.  If RANDOM is non-nil, the tasks
will be ordered randomly.

Tasks are generally items tagged with the ‘TODO’ keyword or the
‘START’ keyword that are not subtasks of an unopened project (an
item with either the ‘START’ keyword or the ‘SHELVED’ keyword).
Media items use the keywords ‘CONSUME’, ‘CONTINUE’, and ‘AGAIN’
keywords, and we again exclude media items contained in unopened
projects.

Note that, since we are selecting only items with specific todo
keywords, MATCH should not include keyword criteria using the ‘/’
suffix."
  (aph/org-agenda-block-match header match
                              '("TODO" "START" "SHELVED"
                                "CONSUME" "CONTINUE" "AGAIN")
                              (not :unopened) limit random))

(defun aph/org-agenda-block-new-projects (header match &optional limit random)
  "Return an agenda block for unopened projects matching MATCH.

The returned block (a list form) defines a todo-type agenda block
with header HEADER that includes only unopened projects that
match the match-string MATCH.

If LIMIT is non-nil, the block will only show that many projects.
If RANDOM is non-nil, the projects will be ordered randomly.

Unopened projects are items tagged with the ‘START’ keyword or
the ‘SHELVED’ keyword.  Since we are selecting only such
projects, MATCH should not include keyword criteria using the ‘/’
suffix."
  (aph/org-agenda-block-match header match
                              '("START" "SHELVED")
                              :unopened limit random))


;;; Custom Agenda Commands
;;;=======================
(setq org-agenda-custom-commands
      `(("1" "Work Agenda"
         (,(aph/org-agenda-block-tagged-agenda "Work Agenda" "work")
          ,(aph/org-agenda-block-tagged-habits "Habits:" "work")
          (tags
           "+work+LEVEL=1+TODO=\"\""
           ((org-agenda-overriding-header "Work Notes:")))
          ,(aph/org-agenda-block-match-tasks "Work Tasks:" "+work")))

        ("2" "Evening Agenda"
         (,(aph/org-agenda-block-tagged-agenda "Evening Agenda" "evening")
          ,(aph/org-agenda-block-tagged-habits "Habits:" "evening")
          ,(aph/org-agenda-block-match-tasks "Evening Tasks:" "+evening")))
        
        ("3" "Weekend Agenda"
         (,(aph/org-agenda-block-tagged-agenda "Weekend Agenda" "weekend")
          ,(aph/org-agenda-block-tagged-habits "Habits:" "weekend")
          ,(aph/org-agenda-block-match-tasks "Weekend Tasks:" "+weekend")))

        ("c" "Computer Agenda" 
         (,(aph/org-agenda-block-tagged-habits "Habits:" "computer")
          ,(aph/org-agenda-block-match-tasks "Open Computer Tasks:"
                                             "+computer" 12)
          ,(aph/org-agenda-block-new-projects "Five Random Emacs Projects"
                                              "+emacs-anki" 5 :random) 
          ,(aph/org-agenda-block-new-projects "Five Random Anki Projects"
                                              "+anki" 5 :random)
          ,(aph/org-agenda-block-new-projects "Five Random AutoHotkey Projects"
                                              "+ahk" 5 :random)
          ,(aph/org-agenda-block-new-projects "Five Random Other Projects"
                                              "+computer-emacs-anki" 5 :random)))

        ("l" "Leisure Agenda"
         (,(aph/org-agenda-block-tagged-habits "Habits:" "leisure")
          ,(aph/org-agenda-block-match-tasks "Leisure Activities:"
                                             "+leisure" 12 :random)))
        
        ("z" . "Meal Activities")
        ("zk" "Knowledge"
         (,(aph/org-agenda-block-tagged-agenda "Meal Agenda: Knowledge"
                                               "meal" :only)
          ,(aph/org-agenda-block-tagged-habits "Habits:"
                                               "meal+knowledge" :only)
          ,(aph/org-agenda-block-match-media "Knowledge Media:"
                                             "meal+knowledge")))
        ("zl" "Leisure"
         (,(aph/org-agenda-block-tagged-agenda "Meal Agenda: Leisure"
                                               "meal" :only)
          ,(aph/org-agenda-block-tagged-habits "Habits:"
                                               "meal+leisure" :only)
          ,(aph/org-agenda-block-match-media "Leisure Media:"
                                             "meal+leisure")))
        ("zo" "Other"
         (,(aph/org-agenda-block-tagged-agenda "Meal Agenda: Other"
                                               "meal" :only)
          ,(aph/org-agenda-block-tagged-habits "Habits:"
                                               "meal-knowledge-leisure" :only)
          ,(aph/org-agenda-block-match-media "Other Media:"
                                             "meal-knowledge-leisure")))
        
        ("r" "Review"
         ((agenda
           ""
           ((org-agenda-overriding-header "Org-Mode Review")
            (org-agenda-ndays 1)
            (org-agenda-sorting-strategy '(time-up category-up))
            (org-agenda-use-time-grid nil)
            (org-habit-show-habits nil)
            (org-agenda-skip-function
             '(aph/org-agenda-skip-tag "review"))))
          ,(aph/org-agenda-block-tagged-agenda
            "Scheduled for Review" "review" :only)
          (tags-todo
           "/OPEN"
           ((org-agenda-overriding-header "Current Projects")))
          (agenda
           ""
           ((org-agenda-overriding-header "Missed Agenda Items")
            (org-agenda-ndays 1)
            (org-agenda-use-time-grid nil)
            (org-agenda-skip-function
             '(or (aph/org-agenda-skip-tag "all")
                  (aph/org-agenda-skip-tag "work")
                  (aph/org-agenda-skip-tag "evening")
                  (aph/org-agenda-skip-tag "review")
                  (aph/org-agenda-skip-tag "weekend")
                  (aph/org-agenda-skip-tag "computer")
                  (aph/org-agenda-skip-tag "leisure")))))
          ,(aph/org-agenda-block-match-tasks
            "Missed Tasks"
            "-work-computer-weekend-review-unfiled-leisure")
          (tags
           "LEVEL=2"
           ((org-agenda-overriding-header "To Be Filed")
            (org-agenda-files `(,(concat org-directory "/capture.org")))))
          (stuck
           ""
           ((org-agenda-overriding-header "Stuck Projects")
            (org-agenda-max-entries 6)
            (org-agenda-sorting-strategy '(category-up))))
          (tags-todo
           "/WAITING"
           ((org-agenda-overriding-header "Stuck Tasks")))
          ,(aph/org-agenda-block-match-tasks "Tasks Without Effort"
                                             "Effort<>{0}-leisure" 8))
         ((org-agenda-dim-blocked-tasks nil)))

        ("p" "Projects"
         ((tags-todo
           "/OPEN"
           ((org-agenda-overriding-header "Open Projects")))
          ,(aph/org-agenda-block-new-projects "Computer Projects"
                                              "+computer" 12 :random)
          ,(aph/org-agenda-block-new-projects "Other Projects"
                                              "-computer-anki"))
         ((org-agenda-dim-blocked-tasks nil)))))


;;; Smart Agenda
;;;=============

;; Variables used to control `aph/org-agenda-display-smart-agenda', below.
(defvar aph/workday-start 10
  "The start of the workday (in hours, according to a 24-hour
clock).  Used by the function
`aph/org-agenda-display-smart-agenda' to display the correct
agenda.")

(defvar aph/workday-end 18
  "The end of the workday (in hours, according to a 24-hour
clock).  Used by the function
`aph/org-agenda-display-smart-agenda' to display the correct
agenda.")

(defun aph/org-agenda-display-smart-agenda ()
  "Display an Org-Mode agenda based on current day and time.

On Saturdays and Sundays, displays the weekend agenda.  On
weekdays, displays the review agenda if the workday (as defined
by the variables `aph/workday-start' and `aph/workday-end')
hasn't started yet, the work agenda if it's in progress, and the
evening agenda if it's already ended."
  (interactive)
  (let ((day            (nth 6 (decode-time)))
        (hour           (nth 2 (decode-time))))
    (if (< 0 day 6)
        (cond
         ((< hour aph/workday-start)  (org-agenda nil "r"))
         ((>= hour aph/workday-end)   (org-agenda nil "2"))
         (t                           (org-agenda nil "1")))
      (org-agenda nil "3"))))

(provide 'init-org-agenda)
