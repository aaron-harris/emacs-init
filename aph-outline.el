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


(provide 'aph-outline)
