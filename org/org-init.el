;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CONFIGURATION
;;;;============================================================================
(require 'org)

;; Enable Org-Mode modules:
(setq org-modules
      '(org-docview
        org-habit
        org-info))

;;; General Settings
;;;=================
(setq org-catch-invisible-edits 'smart)        ; Try to avoid invisible edits.
(setq org-log-into-drawer t)                   ; Use LOGBOOK drawers.
(setq org-track-ordered-property-with-tag t)   ; Use a tag for ORDERED trees.
(setq org-use-speed-commands t)                ; Use speed commands.

;; Setting autofill width.
(add-to-list 'aph/fill-column-by-mode-alist '(org-mode . 76))
(add-hook 'org-mode-hook #'aph/fill-set-column-by-mode)

;;; TODO Keywords
;;;==============
(setq org-use-fast-todo-selection t)

(setq org-todo-keywords
      '((sequence "START(s)" "|" "CANCELLED(c!)")            ; General items
        (sequence "TODO(t)" "WAITING(w@)" "|" "DONE(d!)")    ; Tasks
        (sequence "OPEN(o)" "SHELVED(S!)" "|" "DONE(d!)")    ; Projects
        (sequence "UNREAD(r)" "CONTINUE(-)" "REREAD(R)"
                  "TAG(:)" "|" "FINISHED(f!)")               ; Text media
        (sequence "UNWATCHED(w)" "CONTINUE(-)" "REWATCH(W)"
                  "TAG(:)" "|" "FINISHED(f!)")               ; Video media
        (sequence "UNAVAIL(u)" "BUY(b)" "|" "OWNED(O)")))    ; Media to obtain

;;; Tags
;;;=====

;; Intended meaning of the tags, and which agenda views they show up in.  The at
;; sign indicates that there is a particular agenda view the tag is directed
;; towards.
;;   home: Can only be done at home. (morning, evening, weekend)
;;   work: Shows up in work agenda. (work)
;;   all: Shows up in all agendas.
;;   evening: Shows up in evening agenda. (evening)
;;   weekend: Shows up in weekend agenda. (weekend)
;;   computer: Shows up in computer sub-agendas. (computer)
;;   meal: Can be done while eating. (meal)
;;   review: Shows up in review agenda. (review)
;;   listening: Audio tasks. (listening)
;;   anki: Involves working with Anki.
;;   text: Tags text media.
;;   video: Tags video media.
;;   audio: Tags audio media.
(setq org-tag-alist
      '(("home"      . ?h)
        ("work"      . ?w)
        ("all"       . ?*)
        (:newline    . nil)
        ("evening"   . ?e)
        ("weekend"   . ?s)
        ("computer"  . ?c)
        (:newline    . nil)
        ("review"    . ?r)
        ("meal"      . ?m)
        ("listening" . ?l)
        (:newline    . nil)
        ("anki"      . ?k)
        (:newline    . nil)
        ("text"      . ?t)
        ("video"     . ?v)
        ("audio"     . ?a)))

;; The function org-set-regexps-and-options-for-tags, which sets a buffer-local
;; version of org-tag-alist, seems to screw up :newline entries, replacing
;; (:newline) with ("\n"), which ends up defining a tag with a newline character
;; as its name. This is my crude attempt to fix it (which will probably make it
;; impossible to create buffer-local tags with the #+TAGS directive).
;;
;; TODO: Replace this advice with a more precisely-directed version that will
;;       still allow buffer-local tags.
(unless (advice-member-p #'aph/org-reset-tag-alist
                         'org-set-regexps-and-options-for-tags)
  (advice-add 'org-set-regexps-and-options-for-tags :after
              '(lambda () (kill-local-variable 'org-tag-alist))
              '((name . aph/org-reset-tag-alist))))

;;; Priorities
;;;===========
(setq org-highest-priority ?A)
(setq org-lowest-priority ?E)
(setq org-default-priority ?C)

;;; Properties and Column View
;;;===========================
(setq org-global-properties
      '(("Effort_ALL" . "0:05 0:10 0:15 0:30 1:00 2:00 3:00 4:00 8:00 0")))
(setq org-columns-default-format
      "%50ITEM(Task) %TODO %2PRIORITY(^) %20TAGS(Tags) %6Effort{:} %CLOCKSUM")

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
(setq org-agenda-block-separator (make-string 80 ?=))
(setq org-agenda-remove-tags t)
(setq org-agenda-span 'day)
(setq org-agenda-timegrid-use-ampm t)
(setq org-extend-today-until 4)
(setq org-habit-graph-column 50)
(setq org-agenda-window-setup 'current-window)

;; Format string for agenda items
(setq org-agenda-prefix-format
      '((agenda   . " %i %-13:c%?-12t% s")
        (timeline . "  % s")
        (todo     . " %i %-13:c")
        (tags     . " %i %-13:c")
        (search   . " %i %-13:c")))

;; Settings for timestamps, scheduled items, and deadlines:
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-todo-ignore-with-date 'all)
(setq org-agenda-todo-ignore-scheduled 'all)
(setq org-agenda-todo-ignore-timestamp 'all)
(setq org-agenda-tags-todo-honor-ignore-options t)

;; We designate a project as stuck if it is OPEN and does not have a subtask
;; marked TODO.
(setq org-stuck-projects
      '("/OPEN" ("TODO")))

;; Loading custom agenda commands and capture templates.
(load "org-agenda-init.el")
(load "org-capture-init.el")

;; We want to display our custom agenda automatically on startup.
(add-hook 'after-init-hook
          (lambda ()
            (aph/org-agenda-display-smart-agenda)
            (delete-other-windows)))
