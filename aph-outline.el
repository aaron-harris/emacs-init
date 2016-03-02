;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; OUTLINE EXTENSIONS
;;;;============================================================================

;; Extensions for `outline' module.
(require 'outline)


;;; Information Functions
;;;======================
(defun aph/outline-before-first-heading-p (&optional invisible-ok)
  "Return non-nil if before first visible heading of buffer.
If INVISIBLE-OK is non-nil, also consider invisible headings."
  (condition-case err
      (save-excursion
        (outline-back-to-heading invisible-ok)
        nil)
    (error t)))


(provide 'aph-outline)
