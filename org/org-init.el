;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CONFIGURATION
;;;;============================================================================
(require 'org)

;; We have defined some custom functions useful for org-mode in another file:
(load "org-fun-init.el")

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
        (sequence "NEXT(n)" "FUTURE(F)"
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

;;; Properties and Column View
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
        "~/org/work.org"
        "~/org/media.org"))

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

;; We want to display our custom agenda automatically on startup.
(add-hook 'after-init-hook
          (lambda ()
            (aph/org-agenda-display-smart-agenda)
            (delete-other-windows)))

;; Loading Org submodules.
(load "org-agenda-init.el")
(load "org-capture-init.el")
