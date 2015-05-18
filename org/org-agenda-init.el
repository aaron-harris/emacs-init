;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CUSTOM AGENDA COMMANDS
;;;;============================================================================

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
           "+listening/TODO|NEXT|START"
           ((org-agenda-overriding-header "Listening:")
            (org-agenda-max-entries 6)
            (org-agenda-todo-ignore-scheduled 'future)
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
           "+computer-anki/START"
           ((org-agenda-overriding-header "Five Random Computer Projects")
            (org-agenda-max-entries 5)
            (org-agenda-cmp-user-defined 'aph/random-comparator)
            (org-agenda-sorting-strategy '(user-defined-up))))
          (tags-todo
           "+anki/START"
           ((org-agenda-overriding-header "Five Random Anki Projects")
            (org-agenda-max-entries 5)
            (org-agenda-cmp-user-defined 'aph/random-comparator)
            (org-agenda-sorting-strategy '(user-defined-up))))
          (tags-todo
           "-computer/START"
           ((org-agenda-overriding-header "Non-Computer Projects")))))

        ("z" "Meal"
         ((tags-todo
           "+meal/UNREAD"
           ((org-agenda-overriding-header (concat "Meal Agenda \n"
                                                  org-agenda-block-separator
                                                  "\nThings to Read") )))
          (tags-todo
           "+meal/UNWATCHED"
           ((org-agenda-overriding-header "Things to Watch")))
          (tags-todo
           "+meal/TODO|NEXT"
           ((org-agenda-overriding-header "Things to Do")))
          (tags-todo
           "+meal/-UNREAD-UNWATCHED-TODO-NEXT-UNAVAIL"
           ((org-agenda-overriding-header "Other Tasks")))))))
