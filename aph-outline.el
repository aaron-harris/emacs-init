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


(provide 'aph-outline)
