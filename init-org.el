;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CONFIGURATION
;;;;============================================================================

(require 'aph-org)
(require 'aph-subr)                     ; For `aph/assoc-delete-all'

;; The `provide' statement needs to be at the beginning because
;; `aph/org-emphasis-alist-update' calls `org-reload', and this file
;; will probably be called in the :config block of a `use-package'
;; declaration.
(provide 'init-org)


;;; Basic setup
;;;============
(setq org-modules   '(org-docview org-habit org-info)
      org-directory "~/sync/org")


;;; Editing
;;;========
(setq org-M-RET-may-split-line   nil
      org-catch-invisible-edits  'smart
      org-cycle-emulate-tab      'exc-hl-bol
      org-special-ctrl-a/e       t
      org-use-speed-commands     t)

(add-hook 'org-mode-hook #'visual-line-mode)


;;; Tree Structure
;;;===============
(setq org-blank-before-new-entry '((heading . nil)
                                   (plain-list-item . nil))
      org-log-into-drawer         t)


;;; TODO Items
;;;===========
(setq org-enforce-todo-dependencies       t 
      org-track-ordered-property-with-tag t
      org-use-fast-todo-selection         t)

;; TODO keywords don't display very well in the fast selection buffer
;; if these sequences are longer than three items long (not including
;; "|" entries).  Hence some keywords are out of place; see margin
;; comments.
(setq org-todo-keywords
      '((type "TODO(t)" "ACTIVE(a)" "|" "DONE(d)")    ; also WAIT, HOLD
        (type "MEDIA(m)" "BUY(b)" "|" "REF(r)")       ; also WAIT
        (type "PROJECT(p)" "WAIT(w)" "|" "HOLD(h)"))) ; no WAIT

(advice-add #'org-fast-todo-selection :around #'aph/org-todo-window-advice)


;;; Tags
;;;=====
(setq org-tag-persistent-alist
      '(("home"     . ?h)               ; Can be done only at home
        ("work"     . ?w)               ; Appears in work agenda
        ("calendar" . ?d)               ; Appears in calendar block
        (:newline   . nil)
        ("morning"  . ?m)               ; Appears in morning agenda
        ("evening"  . ?e)               ; Appears in evening agenda
        ("weekend"  . ?s)               ; Appears in weekend agenda
        (:newline   . nil)
        ("computer" . ?c)               ; For computer-related hobbies
        ("review"   . ?r)               ; Periodic review tasks
        ("all"      . ?*)               ; Appears in all "normal" agendas
        (:newline   . nil)
        ("text"     . ?t)               ; Text media
        ("video"    . ?v)               ; Video media
        ("audio"    . ?a)               ; Audio media
        (:newline   . nil)
        ("leisure"  . ?l)               ; Non-productive "tasks"
        ("flag"     . ?F)))             ; Generic "mark for action" 


;;; Priorities
;;;===========
(setq org-highest-priority ?A
      org-lowest-priority  ?E
      org-default-priority ?C)


;;; Properties and Column View
;;;===========================
(setq org-global-properties
      '(("Effort_ALL" . "0:05 0:10 0:15 0:30 1:00 2:00 3:00 4:00 8:00 0")))
(setq org-columns-default-format
      "%50ITEM(Task) %TODO %2PRIORITY(^) %20TAGS(Tags) %6Effort{:} %CLOCKSUM")


;;; Capture
;;;========
(setq org-default-notes-file (concat org-directory "/capture.org")) 


;;; Refile
;;;=======
(setq org-refile-targets '((org-agenda-files :maxlevel . 5)))
(setq org-refile-allow-creating-parent-nodes 'confirm
      org-refile-use-outline-path            'file)


;;; Archive
;;;========
(setq org-archive-location "archive/%s_archive::")


;;; Links
;;;======
(add-hook 'org-store-link-functions #'aph/org-eww-store-link)


;;; Agenda
;;;======= 
(setq org-agenda-files (list org-directory))
(setq org-agenda-block-separator   (make-string 80 ?=)
      org-agenda-dim-blocked-tasks 'invisible
      org-agenda-remove-tags       t
      org-agenda-span              'day
      org-agenda-sticky            t
      org-agenda-timegrid-use-ampm t
      org-agenda-window-setup      'current-window
      org-extend-today-until       4
      org-habit-graph-column       50)
(setq org-agenda-skip-deadline-if-done                 t
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-scheduled-if-done                t
      org-agenda-skip-timestamp-if-done                t
      org-agenda-tags-todo-honor-ignore-options        t
      org-agenda-todo-ignore-timestamp                 nil
      org-agenda-todo-ignore-with-date                 nil)
(setq org-agenda-prefix-format
      '((agenda   . " %i %-13:c%?-12t% s")
        (timeline . "  % s")
        (todo     . " %i %-13:c")
        (tags     . " %i %-13:c")
        (search   . " %i %-13:c")))

;; Make refreshing an agenda preserve the current text scale.
;; (The `with-eval-after-load' is because the advice isn't necessary
;; unless the text scale has been adjusted.)
(with-eval-after-load 'face-remap
  (require 'aph-face-remap)
  (advice-add #'org-agenda-redo :around #'aph/preserving-text-scale))

;; Load custom agenda commands:
(require 'init-org-agenda)


;;; Markup
;;;=======
;; Remove strike-through markup, and change code markup to use ` as a
;; delimiter rather than ~.
(setq org-emphasis-alist (aph/assoc-delete-all "+" org-emphasis-alist))
(aph/org-emphasis-alist-update "~" "`")


;;; Smart Tab Compatibility
;;;========================
(with-eval-after-load 'smart-tab
  (advice-add #'org-cycle :around #'aph/org-cycle-smart-tab-advice))


;;; Face and Display Tweaks
;;;========================
(add-hook 'aph/theme-base-change-hook #'aph/org-update-faces)
