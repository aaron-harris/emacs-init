;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CUSTOM AGENDA COMMANDS
;;;;============================================================================

;;; Pre-Load Default Settings
;;;==========================
;;; For some reason, these variables are not declared before this point, and it
;;; screws up our block-defining functions. Declaring them here makes things
;;; work, but it's rather a kludge.
(setq org-agenda-max-entries nil)

;;; Skip Functions
;;;===============
;;; Here are some custom functions to be used with org-agenda-skip-function.
;;; The initial structure was taken from a stackexchange question, written by
;;; user Jonathan Leech-Pepin.

;; TODO: These functions are still a little quirky. Specifically, they only seem
;;       to work properly when the headline they're called on is visible. Until
;;       this is sorted out, all agenda files should have their default
;;       visibility setting set to CONTENTS or higher.
(defun aph/org-agenda-skip-tag (tag)
  "Returns nil if headline containing point is tagged with TAG, and the
position of the next headline in current buffer otherwise.

Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines tagged with TAG (including by
inheritance)."
  (let ((next-headline
         (save-excursion (or (outline-next-heading)
                                  (point-max))))
        
        (current-headline
         (or (and (org-at-heading-p) (point))
             (save-excursion (org-back-to-heading)))))
    
    (if (member tag (org-get-tags-at current-headline))
        (1- next-headline)
      nil)))

(defun aph/org-agenda-skip-without-tag (tag)
  "Returns nil if headline containing point is not tagged with TAG, and the
position of the next headline in current buffer otherwise.

Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines not tagged with TAG (including by
inheritance)."
  (let ((next-headline
         (save-excursion (or (outline-next-heading)
                             (point-max))))
        
        (current-headline
         (or (and (org-at-heading-p) (point))
             (save-excursion (org-back-to-heading)))))
    
    (if (member tag (org-get-tags-at current-headline))
        nil
      (1- next-headline))))

;;; Block Definitions
;;;==================
;;; For ease of reuse, we define some functions to create custom agenda blocks.

(defun aph/org-agenda-block-tagged-agenda (header tag &optional only)
  "Return a list form defining an agenda block with header HEADER
that includes only items tagged with :all: or TAG, but excludes
habits. If ONLY is non-nil, also exclude :all:-tagged items."       
  `(agenda
    ""
    ((org-agenda-overriding-header ,header)
     (org-agenda-ndays 1)
     (org-agenda-sorting-strategy '(time-up category-up))
     (org-habit-show-habits nil)
     (org-agenda-skip-function
      ',(if only
            `(aph/org-agenda-skip-without-tag ,tag)
          `(and (aph/org-agenda-skip-without-tag ,tag)
                (aph/org-agenda-skip-without-tag "all")))))))

(defun aph/org-agenda-block-tagged-habits (header tag)
  "Return a list form defining an agenda block with header HEADER
that includes only habits tagged with :all: or TAG."
  `(agenda
    ""
    ((org-agenda-overriding-header ,header)
     (org-agenda-entry-types '(:scheduled))
     (org-agenda-skip-function
      '(or (and (aph/org-agenda-skip-without-tag ,tag)
                (aph/org-agenda-skip-without-tag "all"))
           (org-agenda-skip-entry-if 'notregexp ":STYLE:.*habit"))))))

(defun aph/org-agenda-block-match-tasks (header match &optional limit random)
  "Return a list form defining a todo-type agenda block with
header HEADER that includes only tasks that match the
match-string MATCH. If LIMIT is non-nil, the block will only show
that many tasks. If RANDOM is non-nil, the tasks will be ordered
randomly.

Tasks are generally items tagged with the ‘TODO’ keyword that are
not subtasks of an unopened project (an item with the ‘START’
keyword). Other tags that denote specialty tasks (e.g., ‘TAG’)
are also included.

Note that, since we are selecting only tasks, MATCH should not
include keyword criteria using the ‘/’ suffix."
  `(tags-todo
    ,(concat match "/TODO|TAG|START")
    ((org-agenda-overriding-header ,header)
     (org-agenda-skip-function
      '(org-agenda-skip-subtree-if 'todo '("START")))
     ,@(if limit `((org-agenda-max-entries ,limit)))
     ,@(if random
           '((org-agenda-cmp-user-defined 'aph/random-comparator)
             (org-agenda-sorting-strategy '(user-defined-up)))))))

(defun aph/org-agenda-block-new-projects (header match &optional limit random)
  "Return a list form defining a todo-type agenda block with
header HEADER that includes only unopened projects that match the
match-string MATCH.

If LIMIT is non-nil, the block will only show that many
projects. If RANDOM is non-nil, the projects will be ordered
randomly.

Unopened projects are items tagged with the ‘START’ keyword or
the ‘SHELVED’ keyword . Since we are selecting only such
projects, MATCH should not include keyword criteria using the ‘/’
suffix."
  `(tags-todo
    ,(concat match "/START|SHELVED")
    ((org-agenda-overriding-header ,header)
     ,@(if limit `((org-agenda-max-entries ,limit)))
     ,@(if random
           '((org-agenda-cmp-user-defined 'aph/random-comparator)
             (org-agenda-sorting-strategy '(user-defined-up)))))))

;;; Custom Agenda Commands
;;;=======================
(setq org-agenda-custom-commands
      `(("1" "Work Agenda"
         (,(aph/org-agenda-block-tagged-agenda "Work Agenda" "work")
          ,(aph/org-agenda-block-tagged-habits "Habits:" "work")
          (tags
           "+work+LEVEL=1"
           ((org-agenda-overriding-header "Work Notes:")))
          ,(aph/org-agenda-block-match-tasks "Work Tasks:" "+work")
          ,(aph/org-agenda-block-match-tasks "Computer Tasks:" "+computer" 8)))

        ("2" "Evening Agenda"
         (,(aph/org-agenda-block-tagged-agenda "Evening Agenda" "evening")
          ,(aph/org-agenda-block-tagged-habits "Habits:" "evening")
          ,(aph/org-agenda-block-match-tasks "Evening Tasks:" "+evening")
          ,(aph/org-agenda-block-match-tasks "Computer Tasks:" "+computer" 8)))
        
        ("3" "Weekend Agenda"
         (,(aph/org-agenda-block-tagged-agenda "Weekend Agenda" "weekend")
          ,(aph/org-agenda-block-tagged-habits "Habits:" "weekend")
          ,(aph/org-agenda-block-match-tasks "Weekend Tasks:" "+weekend")
          ,(aph/org-agenda-block-match-tasks "Computer Tasks:" "+computer" 8)))

        ("r" "Review"
         ((agenda
           ""
           ((org-agenda-overriding-header "Org-Mode Review")
            (org-agenda-ndays 1)
            (org-agenda-sorting-strategy '(time-up category-up))
            (org-habit-show-habits nil)
            (org-agenda-skip-function
             '(aph/org-agenda-skip-tag "review"))))
          ,(aph/org-agenda-block-tagged-agenda
            "Scheduled for Review" "review" :only)
          (agenda
           ""
           ((org-agenda-overriding-header "Missed Agenda Items")
            (org-agenda-ndays 1)
            (org-agenda-skip-function
             '(or (aph/org-agenda-skip-tag "all")
                  (aph/org-agenda-skip-tag "work")
                  (aph/org-agenda-skip-tag "evening")
                  (aph/org-agenda-skip-tag "weekend")))))
          ,(aph/org-agenda-block-match-tasks
            "Missed Tasks"
            "-work-computer-weekend-review-unfiled")
          (tags
           "LEVEL=2"
           ((org-agenda-overriding-header "To Be Filed")
            (org-agenda-files '("~/org/capture.org"))))
          (stuck
           ""
           ((org-agenda-overriding-header "Stuck Projects")
            (org-agenda-max-entries 6)
            (org-agenda-sorting-strategy '(category-up))))
          (tags-todo
           "/WAITING"
           ((org-agenda-overriding-header "Stuck Tasks")))
          ,(aph/org-agenda-block-match-tasks "Tasks Without Effort"
                                             "Effort<>{0}/TODO|START" 8)))

        ("p" "Projects"
         ((tags-todo
           "/OPEN"
           ((org-agenda-overriding-header "Open Projects")))
          ,(aph/org-agenda-block-new-projects "Five Random Computer Projects"
                                              "+computer-anki" 5 :random)
          ,(aph/org-agenda-block-new-projects "Five Random Anki Projects"
                                              "+anki" 5 :random)
          ,(aph/org-agenda-block-new-projects "Other Projects"
                                              "-computer-anki"))
         ((org-agenda-dim-blocked-tasks nil)))

        ("z" "Meal"
         (,(aph/org-agenda-block-tagged-agenda "Meal Agenda" "meal" :only)
          (tags-todo
           "+meal+text/UNREAD|CONTINUE|REREAD"
           ((org-agenda-overriding-header "Things to Read")))
          (tags-todo
           "+meal+video/UNWATCHED|CONTINUE|REWATCH"
           ((org-agenda-overriding-header "Things to Watch")))
          ,(aph/org-agenda-block-match-tasks "Things to Do" "meal")))))

;; Variables used to control aph/org-agenda-display-smart-agenda, below.
(defvar aph/workday-start 10
  "The start of the workday (in hours, according to a 24-hour
clock). Used by the function aph/org-agenda-display-smart-agenda
to display the correct agenda.")

(defvar aph/workday-end 18
  "The end of the workday (in hours, according to a 24-hour
clock). Used by the function aph/org-agenda-display-smart-agenda
to display the correct agenda.")

(defun aph/org-agenda-display-smart-agenda ()
  "Selects an Org-mode agenda to display, based on the current time and day of the week.

On Saturdays and Sundays, displays the weekend agenda. On
weekdays, displays the review agenda if the workday (as defined
by the variables aph/workday-start and aph/workday-end) hasn't
started yet, the work agenda if it's in progress, and the evening
agenda if it's already ended."
  (interactive)
  (let ((day            (nth 6 (decode-time)))
        (hour           (nth 2 (decode-time))))
    (if (< 0 day 6)
        (cond
         ((< hour aph/workday-start)  (org-agenda nil "r"))
         ((>= hour aph/workday-end)   (org-agenda nil "2"))
         (t                           (org-agenda nil "1")))
      (org-agenda nil "3"))))
