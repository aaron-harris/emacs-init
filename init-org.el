;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CONFIGURATION
;;;;============================================================================

;; According to the Org-Mode documentation, some variables need to be
;; set before Org itself is properly loaded. Thus we only require
;; 'org-install here, and we require 'org at the end of the file, once
;; all our configuration is done.
(require 'org-install)


;;; Basic Setup
;;;============
;; Enable Org-Mode modules:
(setq org-modules
      '(org-docview
        org-habit
        org-info))

(setq org-directory "~/sync/org")


;;; General Settings
;;;=================
(setq org-catch-invisible-edits 'smart)        ; Try to avoid invisible edits.
(setq org-enforce-todo-dependencies t)         ; Make ORDERED tasks matter.
(setq org-log-into-drawer t)                   ; Use LOGBOOK drawers.
(setq org-track-ordered-property-with-tag t)   ; Use a tag for ORDERED trees.
(setq org-use-speed-commands t)                ; Use speed commands.
(setq org-M-RET-may-split-line nil)            ; Better heading insertion.
(setq org-blank-before-new-entry               ; No extraneous blank lines.
      '((heading . nil) (plain-list-item . nil)))


;;; TODO Keywords
;;;==============
(setq org-use-fast-todo-selection t)

(setq org-todo-keywords
      '((sequence "START(s)" "|" "CANCELLED(c!)")            ; General items
        (sequence "TODO(t)" "WAITING(;@)" "|" "DONE(d!)")    ; Tasks
        (sequence "OPEN(o)" "SHELVED(S!)" "|" "DONE(d!)")    ; Projects
        (sequence "UNREAD(r)" "CONTINUE(-)" "REREAD(R)"
                  "TAG(:)" "|" "FINISHED(f!)")               ; Text media
        (sequence "UNWATCHED(w)" "CONTINUE(-)" "REWATCH(W)"
                  "TAG(:)" "|" "FINISHED(f!)")               ; Video media
        (sequence "UNAVAIL(u)" "BUY(b)" "|" "OWNED(O)")))    ; Media to obtain


;;; Tags
;;;=====

;; Intended meaning of the tags, and which agenda views they show up in: 
;;   home: Can only be done at home. (morning, evening, weekend)
;;   work: Shows up in work agenda. (work)
;;   all: Shows up in all agendas.
;;   evening: Shows up in evening agenda. (evening)
;;   weekend: Shows up in weekend agenda. (weekend)
;;   computer: Shows up in computer sub-agendas. (computer)
;;   meal: Can be done while eating. (meal)
;;   review: Shows up in review agenda. (review)
;;   listening: Audio tasks. (listening)
;;   emacs: Involves tweaking Emacs configuration.
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
        ("emacs"     . ?x)
        (:newline    . nil)
        ("text"      . ?t)
        ("video"     . ?v)
        ("audio"     . ?a)))

;; TODO: Replace this advice with a more precisely-directed version that will
;;       still allow buffer-local tags.
(defun aph/org-reset-tag-alist ()
  "Advice to block spurious :newline tags when using `org-tag-alist'.

The function `org-set-regexps-and-options-for-tags', which sets a
buffer-local version of `org-tag-alist', seems to screw
up :newline entries, replacing (:newline) with (\"\n\"), which
ends up defining a tag with a newline character as its name. This
is a crude attempt to fix it (which will probably make it
impossible to create buffer-local tags with the #+TAGS
directive)."
  (kill-local-variable 'org-tag-alist))

(advice-add #'org-set-regexps-and-options-for-tags :after
            #'aph/org-reset-tag-alist)


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
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Miscellaneous settings:
(setq org-refile-use-outline-path 'file)

;; Load capture templates:
(require 'init-org-capture)


;;; Links
;;;======
(defun aph/org-eww-store-link ()
      "Store the current eww url as an Org-Mode link."
      (when (eq major-mode 'eww-mode)
        (org-store-link-props
         :type         "http"
         :link         (eww-current-url)
         :description  (plist-get eww-data :title))))

(add-hook 'org-store-link-functions #'aph/org-eww-store-link)


;;; Agenda
;;;=======
;; Agenda files:
(setq org-agenda-files
      (mapcar (apply-partially #'concat org-directory "/")
              '("capture.org"
                "personal.org"
                "home.org"
                "computer.org"
                "emacs.org"
                "languages.org"
                "math.org"
                "programming.org"
                "shopping.org"
                "social.org"
                "work.org"
                "media.org")))

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

;; Loading custom agenda commands.
(require 'init-org-agenda)

;; We want to display our custom agenda automatically on startup.
(add-hook 'after-init-hook
          (lambda ()
            (aph/org-agenda-display-smart-agenda)
            (delete-other-windows)))
;;; Mobile
;;;=======
(require 'org-mobile)

(setq org-mobile-directory "~/sync/mobile")
(setq org-mobile-inbox-for-pull (concat org-directory "/capture.org"))

(if (eq aph/machine 'mpc)
    (setq org-mobile-checksum-binary
          "C:/Program Files (Portable)/GnuWin Core Utilities/bin/sha1sum.exe"))

(require 'org)           ; Finish loading Org-Mode.
(provide 'init-org) 
