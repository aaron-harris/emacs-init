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
                      #'outline-previous-visible-heading)))
    (cond
     ((< arg 0)           (aph/outline--*-heading
                           (- arg) (not dir) invisible-ok))
     ((not invisible-ok)  (funcall vis-func arg))
     (:else               (dotimes (i arg (point))
                            (funcall univ-func))))))

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


(provide 'aph-outline)
