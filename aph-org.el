;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS - ORG MODE
;;;;============================================================================

(require 'org)
(require 'aph-advice)             ; For `aph/with-advice'
(require 'aph-lexical)            ; For `aph/defun-dyn'
(require 'aph-dash)               ; For `->>', `aph/reductions'


;;; Heading Structure
;;;==================
(defun aph/org-count-children (&optional pom)
  "Return the number of children for Org mode heading at POM.

If POM (a number or a marker) is omitted, use point. If POM is
not in a heading, return nil."
  (let ((pom (or pom (point))))
    (save-excursion
      (goto-char pom)
      (unless (org-before-first-heading-p)
        (org-back-to-heading :invisible-ok)
        (if (org-goto-first-child)
            (let ((acc  1))
              (while (org-get-next-sibling)
                (setq acc (1+ acc)))
              acc)
          0)))))

(defun aph/org-goto-nth-child (n)
  "Goto the Nth child of heading at point.

Children are counted from 1. If heading does not have N children,
return nil and do not move point; otherwise, return point.

If N is zero, call `org-back-to-heading' and return point.

If N is negative, goto the (-N)th child from the end (so
(aph/org-goto-nth-child -1) moves to the last child)."
  (cond ((zerop n) (progn (org-back-to-heading)
                        (point))) 
        ((< n 0)   (aph/org-goto-nth-child
                  (+ (aph/org-count-children) (1+ n)))) 
        ((> n 0)
         (let ((target (catch 'fail
                         (save-excursion
                           (unless (org-goto-first-child)
                             (throw 'fail nil))
                           (dotimes (i (1- n) (point))
                             (unless (org-get-next-sibling)
                               (throw 'fail nil)))))))
           (when target
             (goto-char target))))))


;;; Properties
;;;===========
(defun aph/org-get-property-of-children
    (pom prop &optional inherit literal-nil)
  "Return list of PROP values for all children of heading at POM.
See `org-entry-get' for use of optional parameters."
  (save-excursion
    (goto-char pom)
    (unless (org-goto-first-child) '())
    (let ((acc (list (org-entry-get (point) prop inherit literal-nil))))
      (while (org-get-next-sibling)
        (push (org-entry-get (point) prop inherit literal-nil) acc))
      (nreverse acc))))

(defun aph/org-sum-property-of-children
    (pom prop &optional inherit)
  "Return sum of PROP values for all children of heading at POM.

If INHERIT is non-nil, use inherited values for PROP. Ignore
non-numeric values." 
  (->> (aph/org-get-property-of-children pom prop inherit)
       (mapcar #'string-to-number)
       (apply #'+)))


;;; Match Strings
;;;==============
(aph/defun-dyn aph/org-match-at-point-p (match &optional todo-only)
  "Return non-nil if headline at point matches MATCH.
Here MATCH is a match string of the same format used by
`org-tags-view'.

If the optional argument TODO-ONLY is non-nil, do not declare a
match unless headline at point is a todo item."
  (let ((todo      (org-get-todo-state))
        (tags-list (org-get-tags-at)))
    (eval (cdr (org-make-tags-matcher match)))))


;;; Editing Commands
;;;=================
(defun aph/org-kill-line ()
  "As `org-kill-line', but clear table rows.

If point is inside an Org table, call
`aph/org-table-clear-row-forward'; otherwise, defer to
`org-kill-line'."
  (interactive) 
  (call-interactively
   (if (org-table-p)
       (progn (require 'aph-org-table)
              #'aph/org-table-clear-row-forward)
     #'org-kill-line)))

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
(defun aph/org-cycle-smart-tab-advice (fn &optional arg)
  "Advice to make `org-cycle' use `smart-tab'.

With this advice :around `org-cycle', that function will use
`smart-tab' as its fallback action instead of just indenting.
All other behavior of `org-cycle' remains unchanged."
  (aph/with-advice
      ;; Make `org-cycle' use `smart-tab' as fallback action.
      ((#'global-key-binding :before-until
                             (lambda (keys &optional accept-default)
                               (when (equal keys "\t")
                                 #'smart-tab)))
       ;; Prevent `smart-tab' from using `org-cycle' as its fallback.
       (#'smart-tab-default :override #'indent-for-tab-command))
    (apply fn arg)))

(defun aph/org-update-faces ()
  "Update definition of `org-hide' to match current theme.
Run after changing themes to fix display problems with the
`org-hide' face."
  (let ((foreground (org-find-invisible-foreground)))
    (if foreground
        (set-face-foreground 'org-hide foreground))))

(provide 'aph-org)
