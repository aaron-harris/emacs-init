;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG-MODE CONFIGURATION
;;;;============================================================================

(require 'aph-subr)                     ; For `aph/assoc-delete-all'


;;; Basic setup
;;;============
(validate-setq org-modules      '(org-docview org-habit org-info)
	       org-directory    "~/sync/org"
	       org-agenda-files (list org-directory))


;;; Editing
;;;========
(validate-setq org-M-RET-may-split-line   nil
	       org-catch-invisible-edits  'smart
	       org-cycle-emulate-tab      'exc-hl-bol
	       org-special-ctrl-a/e       t
	       org-special-ctrl-k         t
	       org-use-speed-commands     t)

(add-hook 'org-mode-hook #'visual-line-mode)


;;; Tree Structure
;;;===============
(validate-setq
 org-blank-before-new-entry '((heading . nil)
			      (plain-list-item . nil))
 org-log-into-drawer         t)


;;; TODO Items
;;;===========
(validate-setq org-track-ordered-property-with-tag t
	       org-use-fast-todo-selection         t)

;; TODO keywords don't display very well in the fast selection buffer
;; if these sequences are longer than three items long (not including
;; "|" entries).  Hence some keywords are out of place; see margin
;; comments.
(setq org-todo-keywords
      '((type "TODO(t)" "ACTIVE(a)" "|" "DONE(d)")    ; also WAIT, HOLD
	(type "MEDIA(m)" "BUY(b)" "|" "REF(r)")	      ; also WAIT
	(type "PROJECT(p)" "WAIT(w)" "|" "HOLD(h)"))) ; no WAIT


;;; Tags
;;;=====
(setq org-tag-persistent-alist
      '(("home"     . ?h)	     ; Can be done only at home
	("work"     . ?w)	     ; Appears in work agenda
	("calendar" . ?d)	     ; Appears in calendar block
	(:newline   . nil)	     ;
	("morning"  . ?m)	     ; Appears in morning agenda
	("evening"  . ?e)	     ; Appears in evening agenda
	("weekend"  . ?s)	     ; Appears in weekend agenda
	(:newline   . nil)	     ;
	("computer" . ?c)	     ; For computer-related hobbies
	("review"   . ?r)	     ; Periodic review tasks
	("all"      . ?*)	     ; Appears in all "normal" agendas
	(:newline   . nil)	     ;
	("text"     . ?t)	     ; Text media
	("video"    . ?v)	     ; Video media
	("audio"    . ?a)	     ; Audio media
	(:newline   . nil)	     ;
	("leisure"  . ?l)	     ; Non-productive "tasks"
	("flag"     . ?F)))	     ; Generic "mark for action"


;;; Priorities
;;;===========
(validate-setq org-highest-priority ?A
	       org-lowest-priority  ?E
	       org-default-priority ?C)


;;; Properties and Column View
;;;===========================
(validate-setq
 org-global-properties
 '(("Effort_ALL" . "0:05 0:10 0:15 0:30 1:00 2:00 3:00 4:00 8:00 0"))
 
 org-columns-default-format
 "%50ITEM(Task) %TODO %2PRIORITY(^) %20TAGS(Tags) %6Effort{:} %CLOCKSUM")


;;; Capture
;;;========
(validate-setq
 org-default-notes-file (concat org-directory "/capture.org"))


;;; Refile
;;;=======
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 5))))
(validate-setq
 org-refile-allow-creating-parent-nodes 'confirm
 org-refile-use-outline-path            'file)


;;; Archive
;;;========
(validate-setq org-archive-location "archive/%s_archive::")

(provide 'init-org)
;; init-org.el ends here
