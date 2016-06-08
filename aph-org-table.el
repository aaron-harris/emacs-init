;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS - ORG MODE TABLES
;;;;============================================================================

(require 'org-table)


;;; Subroutines
;;;============
(defun aph/org-table-end-of-this-field ()
  "As `org-table-end-of-field', but never change fields.
If already at or after the end of the current field, do not move
point."
  (interactive)
  (unless (and (org-table-p)
               (looking-at-p " +|"))
    (org-table-end-of-field 1)))


;;; Editing Functions
;;;==================
;;;###autoload
(defun aph/org-table-clear-row-forward ()
  "Erase contents of table cells from point to end of row.

If point is not inside an Org table, signal an error."
  (interactive)
  (unless (org-table-p)
    (error "Not in Org table"))
  (save-excursion
    (save-match-data
      (while (re-search-forward "[^|]" (point-at-eol) :noerror)
        (replace-match " ")))))


(provide 'aph-org-table)
