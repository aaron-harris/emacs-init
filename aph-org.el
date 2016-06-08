;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS - ORG MODE
;;;;============================================================================

(require 'org)
(require 'vizier)			; For `aph/with-advice'


;;; Editing Commands
;;;=================
(defun aph/org-cycle-with-smart-tab (&optional arg)
  "As `org-cycle', but fall back to `smart-tab'.

Normally, if the command `org-cycle' can find nothing to do, it
falls back to the global binding for TAB, subject to the option
`org-cycle-emulate-tab'.  This command behaves the same as
`org-cycle', except it falls back to `smart-tab' instead if
`smart-tab-mode' is enabled."
  (interactive "P")
  (vizier-with-advice-if (bound-and-true-p smart-tab-mode) 
      ;; Make `org-cycle' use `smart-tab' as fallback action.
      ((global-key-binding
	:before-until
	(lambda (keys &optional accept-default)
	  (when (equal keys "\t") #'smart-tab)))
       ;; Prevent `smart-tab' from using `org-cycle' as its fallback.
       (smart-tab-default :override #'indent-for-tab-command))
    (org-cycle arg))) 

(defun aph/org-increase-number (&optional inc)
  "As `org-increase-number-at-point', but more flexible.

As `org-increase-number-at-point', but first reposition point
within a table cell.  Specifically, when inside an Org table and
not on a number, move to the end of the cell.  This handles the
typical case where the cell contains only a right-justified
number and point is at the beginning of the cell (on a leading
space)."
  (interactive "p")
  (when (and (org-table-p)
             (not (number-at-point)))
    (aph/org-table-end-of-this-field))
  (org-increase-number-at-point inc))

(defun aph/org-decrease-number (&optional inc)
  "As `org-decrease-number-at-point', but more flexible.
See `aph/org-increase-number' for more details."
  (interactive "p")
  (aph/org-increase-number (- (or inc 1))))


;;; Refile
;;;=======

;;;###autoload
(defun aph/org-goto-last-refile ()
  "Goto last Org-mode item refiled.

This has the same effect as supplying a C-u C-u prefix argument
to `org-agenda-refile'.  It is intended for use globally, where a
keybinding for that function is not appropriate."
  (interactive)
  (org-agenda-refile '(16)))



;;; Links
;;;======

;;;###autoload
(defun aph/org-eww-store-link ()
      "Store the current eww url as an Org-Mode link."
      (when (eq major-mode 'eww-mode)
        (org-store-link-props
         :type         "http"
         :link         (eww-current-url)
         :description  (plist-get eww-data :title))))


;;; Advice
;;;=======
(defun aph/org-update-faces ()
  "Update definition of `org-hide' to match current theme.
Run after changing themes to fix display problems with the
`org-hide' face."
  (let ((foreground (org-find-invisible-foreground)))
    (if foreground
        (set-face-foreground 'org-hide foreground))))

(provide 'aph-org)
