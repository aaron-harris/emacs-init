;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CONFIGURATION
;;;;============================================================================
(require 'org)

;;; Modules Enabled
;;;================
(setq org-modules
      '(org-docview
        org-habit
        org-info))

;;; General Settings
;;;=================
(setq org-catch-invisible-edits 'smart) ; Try to avoid invisible edits.
(setq org-log-into-drawer t) ; Use LOGBOOK drawers.
(setq org-track-ordered-property-with-tag t) ; Use a tag for ORDERED trees.
(setq org-use-speed-commands t) ; Use speed commands.

;;; TODO Keywords
;;;==============
(setq org-use-fast-todo-selection t)

(setq org-todo-keywords
      '(;; Tasks
        (sequence "TODO(t)" "IN-PROGRESS(p!)" "WAITING(w@)" "APPT(a!)"
                  "|" "DONE(d!)" "CANCELLED(c!)" "DEFERRED(>!)")
        
        ;; Projects
        (sequence "START(1)" "OPEN(o!)"
                  "|" "FINISHED(f!)" "SHELVED(s!)")

        ;; Subtasks
        (sequence "FUTURE(F)" "NEXT(n)"
                  "|" "DONE(d!)")))

;;; Tags
;;;=====

;; Intended meaning of the tags:
;;   home: Can only be done at home.
;;   work: Shows up in work agenda.
;;   errand: To be done outside home and work.
;;   evening: Shows up in evening agenda.
;;   winddown: Can be done during winddown.
;;   weekend: Should only show up in weekend agenda.
;;   computer: Requires only a computer.
;;   meal: Can be done while eating.
;;   listening: Audio tasks (can be done while working).

;; Defining this here (or using customization) doesn't seem to be working at the
;; moment, so all org mode files should include these lines until things get
;; resolved.
;;    #+TAGS: { work(w) home(h) errand(e) }
;;    #+TAGS: evening(v) winddown(d) weekend(s)
;;    #+TAGS: computer(c) meal(m) listening(l) review(r)
(setq org-tag-alist nil)
(setq org-tag-persistent-alist nil)

;;; Priority
;;;=========
(setq org-highest-priority ?A)
(setq org-lowest-priority ?E)
(setq org-default-priority ?C)

;;; Properties and Column Mode
;;;===========================
(setq org-global-properties
      '(("Effort_ALL" . "0:05 0:10 0:15 0:20 0:30 1:00 2:00 4:00 8:00 0")))
(setq org-columns-default-format
      "%50ITEM(Task) %TODO %2PRIORITY(^) %10TAGS(Tags) %6Effort{:} %CLOCKSUM")

;;; Capture, Refile, and Archive
;;;=============================
;; Default locations and targets:
(setq org-default-notes-file (concat org-directory "/capture.org"))
(setq org-archive-location "archive/%s_archive::")
(setq org-refile-targets '((org-agenda-files :maxlevel . 5)))

;; Miscellaneous settings:
(setq org-refile-use-outline-path 'file)

;; Capture templates:
(setq org-capture-templates
      '(("n" "Note" entry
         (file+headline org-default-notes-file "Notes")
         "* %?
:LOGBOOK:
- Captured                             %U
:END:"
         :kill-buffer)

        ("N" "Link" entry
         (file+headline org-default-notes-file "Notes")
         "* %A%?
:LOGBOOK:
- Captured                             %U
:END:"
         :kill-buffer)

        ("q" "Clip" entry
         (file+headline org-default-notes-file "Notes")
         "* %A%?\n  %i
:LOGBOOK:
- Captured                             %U
:END:"
         :kill-buffer)

        ("Q" "Clip (with Link)" entry
         (file+headline org-default-notes-file "Notes")
         "* %A%?\n  %i
:LOGBOOK:
- Captured                             %U
:END:"
         :kill-buffer)

        ("t" "Task" entry
         (file+headline org-default-notes-file "Tasks")
         "* TODO %^{Effort}p%?
:LOGBOOK:
- Captured                             %U
:END:
:PROPERTIES:
:END:"
         :kill-buffer)

        ("T" "Task (with Link)" entry
         (file+headline org-default-notes-file "Tasks")
         "* TODO %^{Effort}p%?%A.
:LOGBOOK:
- Captured                             %U
:END:
:PROPERTIES:
:END:"
         :kill-buffer)

        ("p" "Project" entry
         (file+headline org-default-notes-file "Projects")
         "* START %?
:LOGBOOK:
- Captured                             %U
:END:"
         :kill-buffer)

        ("P" "Project (with Link)" entry
         (file+headline org-default-notes-file "Projects")
         "* START %?%A.
:LOGBOOK:
- Captured                             %U
:END:"
         :kill-buffer)

        ("g" "Grocery List Item" item
         (file+headline "~/org/shopping.org" "Grocery List")
         "[ ] %?"
         :unnarrowed
         :kill-buffer)

        ("s" "Shopping List Item" item
         (file+headline "~/org/shopping.org" "Shopping List")
         "[ ] %?"
         :unnarrowed
         :kill-buffer)
        
        ("w" "Password" table-line
         (file "~/org/passwords.org")
         "| %^{Service} | %^{Username|meerwolf@gmail.com} | %^{Length} | %^{Comments|epo}"
         :immediate-finish
         :kill-buffer)))

;;; Agenda
;;;=======
;; Agenda files:
(setq org-agenda-files
      '("~/org/capture.org"
        "~/org/personal.org"
        "~/org/home.org"
        "~/org/computer.org"
        "~/org/emacs.org"
        "~/org/languages.org"
        "~/org/math.org"
        "~/org/programming.org"
        "~/org/shopping.org"
        "~/org/social.org"
        "~/org/work.org"))

;; General agenda settings:
(setq org-agenda-block-separator (make-string 80 ?=)) ; Block agenda separator.
(setq org-agenda-remove-tags t) ; Do not show tags in the agenda.
(setq org-agenda-span 'day) ; Default to day view.
(setq org-agenda-timegrid-use-ampm t) ; Use AM/PM in the agenda time grid.
(setq org-extend-today-until 4) ; The day starts at 4 AM.
(setq org-habit-graph-column 50) ; The column to show the habit graph at.
(setq org-agenda-window-setup 'current-window) ; Show agenda in current window.

;; Settings for timestamps, scheduled items, and deadlines:
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-todo-ignore-scheduled 'all)
(setq org-agenda-tags-todo-honor-ignore-options t)


;; Defining stuck projects:
(setq org-stuck-projects
      '("/OPEN|BLOCKED"          ; Projects are OPEN or BLOCKED.
        ("NEXT" "IN-PROGRESS"))) ; Projects are not stuck if they have a NEXT
                                 ; subtask or a task in progress.

;; A function to skip tags in agenda view.  Modified from a function found on
;; stackexchange, written by user Jonathan Leech-Pepin.
;; FIXME - This is still under construction.
(defun aph/org-agenda-skip-tag (tag)
  "Returns nil if headline containing point is tagged with TAG, and the
position of the next headline in current buffer otherwise.

Intended for use with org-agenda-skip-function, where this will skip exactly
those headlines tagged with TAG (including by inheritance)."
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

Intended for use with org-agenda-skip-function, where this will skip exactly
those headlines not tagged with TAG (including by inheritance)."
  (let ((next-headline
         (save-excursion (or (outline-next-heading)
                             (point-max))))
        
        (current-headline
         (or (and (org-at-heading-p) (point))
             (save-excursion (org-back-to-heading)))))
    
    (if (member tag (org-get-tags-at current-headline))
        nil
      (1- next-headline))))

;; Custom Agenda Commands.
(setq org-agenda-custom-commands
      '(("1" "Work Agenda"
         ((agenda
           ""
           ((org-agenda-overriding-header "Work Agenda")
            (org-agenda-ndays 1)
            (org-agenda-sorting-strategy '(time-up
                                           category-up))
            (org-agenda-skip-function
             '(or (aph/org-agenda-skip-tag "home")
                  (aph/org-agenda-skip-tag "errand")
                  (aph/org-agenda-skip-tag "review")))))
          (tags
           "+work-review+LEVEL=1"
           ((org-agenda-overriding-header "Work Notes:")))
          (tags-todo
           "work"
           ((org-agenda-overriding-header "Work Tasks:")))
          (tags-todo
           "listening/TODO|NEXT|START"
           ((org-agenda-overriding-header "Listening:")
            (org-agenda-max-entries 6)
            (org-agenda-skip-function
             '(org-agenda-skip-subtree-if 'todo '("START")))))
          (tags-todo
           (concat "computer+work-meal-listening-review"
                   "|computer+any-meal-listening-review"
                   "|computer-home-meal-listening-review"
                   "/TODO|NEXT|START")
           ((org-agenda-overriding-header "Computer Tasks:")
            (org-agenda-max-entries 10)
            (org-agenda-sorting-strategy '(priority-down
                                           effort-up
                                           category-up))
            (org-agenda-skip-function
             '(org-agenda-skip-subtree-if 'todo '("START")))))))

        ("2" "Evening Agenda"
         ((agenda
           ""
           ((org-agenda-overriding-header "Evening Agenda")
            (org-agenda-ndays 1)
            (org-habit-show-habits nil)
            (org-agenda-sorting-strategy '(time-up
                                           category-up))
            (org-agenda-skip-function
             '(aph/org-agenda-skip-tag "work"))))
          (agenda
           ""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-entry-types '(:scheduled))
            (org-agenda-skip-function
             '(or
               (aph/org-agenda-skip-without-tag "evening")
               (org-agenda-skip-entry-if 'notregexp ":STYLE:.*habit")))))
          (tags-todo
           "+evening-winddown"
           ((org-agenda-overriding-header "Evening Tasks:")
            (org-agenda-sorting-strategy '(priority-down
                                           effort-up
                                           category-up))
            (org-agenda-skip-function
             '(org-agenda-skip-subtree-if 'todo '("START")))))
          (tags-todo
           "+winddown"
           ((org-agenda-overriding-header "Winddown Tasks:")
            (org-agenda-sorting-strategy '(priority-down
                                           effort-up
                                           category-up))
            (org-agenda-skip-function
             '(org-agenda-skip-subtree-if 'todo '("START")))))
          (tags-todo
           "computer-meal-listening/TODO|NEXT|START"
           ((org-agenda-overriding-header "Computer Tasks:")
            (org-agenda-max-entries 5)
            (org-agenda-sorting-strategy '(priority-down
                                           effort-up
                                           category-up))
            (org-agenda-skip-function
             '(org-agenda-skip-subtree-if 'todo '("START")))))))
        
        ("3" "Weekend Agenda"
         ((agenda
           ""
           ((org-agenda-overriding-header "Weekend Agenda")
            (org-agenda-ndays 1)
            (org-habit-show-habits nil)
            (org-agenda-sorting-strategy '(time-up
                                           category-up))
            (org-agenda-skip-function
             '(aph/org-agenda-skip-tag "work"))))
          (agenda
           ""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-entry-types '(:scheduled))
            (org-agenda-skip-function
             '(or (aph/org-agenda-skip-without-tag "weekend")
                  (org-agenda-skip-entry-if 'notregexp ":STYLE:.*habit")))))
          (tags-todo
           "weekend/TODO|NEXT|START"
           ((org-agenda-overriding-header "Weekend Tasks:")
            (org-agenda-sorting-strategy '(priority-down
                                           effort-up
                                           category-up))
            (org-agenda-skip-function
             '(org-agenda-skip-subtree-if 'todo '("START")))))
          (tags-todo
           "computer-meal-listening/TODO|NEXT|START"
           ((org-agenda-overriding-header "Computer Tasks:")
            (org-agenda-max-entries 6)
            (org-agenda-sorting-strategy '(priority-down
                                           effort-up
                                           category-up))
            (org-agenda-skip-function
             '(org-agenda-skip-subtree-if 'todo '("START")))))))
        
        ("r" "Review"
         ((agenda
           ""
           ((org-agenda-overriding-header "Org-Mode Review")
            (org-habit-show-habits nil)
            (org-agenda-sorting-strategy '(time-up
                                           category-up))))
          (agenda
           ""
           ((org-agenda-overriding-header "Review Tasks")
            (org-agenda-skip-function
             '(aph/org-agenda-skip-without-tag "review"))))
          (tags
           "LEVEL=2"
           ((org-agenda-overriding-header "To Be Filed:")
            (org-agenda-files '("~/org/capture.org"))))
          (stuck
           ""
           ((org-agenda-overriding-header "Stuck Projects:")
            (org-agenda-max-entries 6)
            (org-agenda-sorting-strategy '(category-up))))
          (tags-todo
           "/WAITING"
           ((org-agenda-overriding-header "Stuck Tasks:")))
          (tags-todo
           "Effort<>{0}/TODO|NEXT|START"
           ((org-agenda-overriding-header "Tasks Without Effort:")
            (org-agenda-max-entries 8)
            (org-agenda-skip-function
             '(org-agenda-skip-subtree-if 'todo '("START")))))))

        ("p" "Projects"
         ((tags-todo
           "/OPEN"
           ((org-agenda-overriding-header "Open Projects")))
          (tags-todo
           "+computer/START"
           ((org-agenda-overriding-header "Computer Projects")))
          (tags-todo
           "-"
           ((org-agenda-overriding-header "Other Projects")))))))

;; Display an agenda on startup, based on current day and time. On weekends,
;; this is the weekend agenda. On weekdays, use the review agenda in the
;; morning, the work agenda during the day, and evening agenda in the evening.
(add-hook 'after-init-hook
          (lambda ()
            (let* ((day            (nth 6 (decode-time)))
                   (hour           (nth 2 (decode-time)))
                   (workday-start  10)
                   (workday-end    18))
              (if (< 0 day 6)
                  (cond
                   ((< hour workday-start) (org-agenda nil "r"))
                   ((> hour workday-end)   (org-agenda nil "2"))
                   (t                      (org-agenda nil "1")))
                (org-agenda nil "3"))
              (delete-other-windows))))
