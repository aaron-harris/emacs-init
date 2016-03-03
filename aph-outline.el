;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; OUTLINE EXTENSIONS
;;;;============================================================================

;; Extensions for `outline' module.
(require 'outline)


;;; Information Functions
;;;======================
(defun aph/outline-before-first-heading (&optional invisible-ok)
  "Return non-nil if before first visible heading of buffer.
If INVISIBLE-OK is non-nil, also consider invisible headings.

Set match data for `outline-level'."
  ;; The match data is set implicitly by `outline-back-to-heading'.
  (condition-case err
      (save-excursion
        (outline-back-to-heading invisible-ok)
        nil)
    (error t)))

(defun aph/outline-before-first-heading-p (&optional invisible-ok)
  "As `aph/outline-before-first-heading', but save match data."
  (save-match-data (aph/outline-before-first-heading invisible-ok)))

(defun aph/outline-level ()
  "As function in variable `outline-level', but more flexible.

- When called at the beginning of a heading, behave as
  (funcall outline-level).
- When called before the first heading of the buffer, return 0.
- Otherwise, return the value (funcall outline-level) would return if
  called at the beginning of current heading.

In any case, do not assume that match data reflects
`outline-regexp'."
  (if (aph/outline-before-first-heading :invisible-ok) 0
   (save-excursion
     (outline-back-to-heading :invisible-ok)
     (funcall outline-level))))


;;; Navigation Functions
;;;=====================
(defun aph/outline--*-heading (arg dir &optional invisible-ok)
  "Used by `aph/outline-next-heading', `aph/outline-previous-heading'.
If DIR is non-nil, behave as `aph/outline-next-heading';
otherwise, behave as `aph/outline-previous-heading'."
  (let ((univ-func  (if dir #'outline-next-heading
                      #'outline-previous-heading))
        (vis-func   (if dir #'outline-next-visible-heading
                      #'outline-previous-visible-heading))
        (start      (point)))
    (cond
     ((< arg 0)           (aph/outline--*-heading
                           (- arg) (not dir) invisible-ok))
     ((not invisible-ok)  (funcall vis-func arg))
     (:else               (dotimes (i arg (point))
                            (funcall univ-func))))
    (unless (= start (point)) (point))))

(defun aph/outline-next-heading (arg &optional invisible-ok)
  "As `outline-next-visible-heading', but maybe invisible too.
If INVISIBLE-OK is non-nil, call `outline-next-heading' ARG
times (or `outline-previous-heading' -ARG times if ARG is
negative).  Otherwise, defer to `outline-next-visible-heading'."
  (interactive "p")
  (aph/outline--*-heading arg t invisible-ok))

(defun aph/outline-previous-heading (arg &optional invisible-ok)
  "As `outline-previous-visible-heading', but maybe invisible too.
If INVISIBLE-OK is non-nil, call `outline-previous-heading' ARG
times (or `outline-previous-heading' -ARG times if ARG is
negative).  Otherwise, defer to `outline-previous-visible-heading'."
  (interactive "p")
  (aph/outline--*-heading arg nil invisible-ok))

(defun aph/outline-get-first-child (&optional invisible-ok)
  "Move to first visible child of the current heading, and return point.
If no such heading, return nil and do not move point.

If INVISIBLE-OK is non-nil, also consider invisible children." 
  (if (aph/outline-before-first-heading-p invisible-ok)
      (aph/outline-next-heading 1 invisible-ok)
    (let ((start  (point))
          (level  (aph/outline-level)))
      (aph/outline-next-heading 1 invisible-ok)
      (if (and (outline-on-heading-p invisible-ok)
               (> (aph/outline-level) level))
          (point)
        (goto-char start)
        nil))))

(defun aph/outline-down-heading (arg &optional invisible-ok)
  "Move to the first visible child of the current heading.
With argument, descend ARG levels.
If INVISIBLE-OK is non-nil, also consider invisible lines.

If ARG is negative, move up ARG levels instead; note that this is
a departure from the behavior of `outline-up-heading'."
  (interactive "p")
  (and (eq this-command 'aph/outline-down-heading)
       (or (eq last-command 'aph/outline-down-heading)
           (push-mark)))
  (if (< arg 0)
      (outline-up-heading (- arg) invisible-ok)
    (while (and (> arg 0)
                (aph/outline-get-first-child))
      (setq arg (1- arg)))))

(defun aph/outline-get-next-sibling ()
  "As `outline-get-next-sibling', with less state-dependence.

The function `outline-get-next-sibling' requires that the current
match data reflects `outline-regexp' and that point is currently
at the beginning of a heading.  This function performs the same
function without those requirements.

If point is currently before the first heading in the buffer,
then do not move point and return nil."
  (unless (aph/outline-before-first-heading :invisible-ok)
    (outline-get-next-sibling)))

(defun aph/outline-get-last-child ()
  "Move to last child of the current heading, and return point.
If no such heading, return nil and do not move point."
  (when (aph/outline-get-first-child)
    (let ((pos (point)))
      (while (aph/outline-get-next-sibling)
        (setq pos (point)))
      (goto-char pos))))

(defun aph/outline-down-heading-from-end (arg)
  "Move to the last child of the current heading.
With argument, descend ARG levels in this way.

If ARG is negative, move up ARG levels instead."
  (interactive "p")
  (and (eq this-command 'aph/outline-down-heading-from-end)
       (or (eq last-command 'aph/outline-down-heading-from-end)
           (push-mark)))
  (if (< arg 0)
      (outline-up-heading (- arg) :invisible-ok)
    (while (and (> arg 0)
                (aph/outline-get-last-child))
      (setq arg (1- arg)))))


(provide 'aph-outline)
