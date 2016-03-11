;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS - ORG MODE TABLES
;;;;============================================================================

(require 'org-table)


;;; Editing Functions
;;;==================
(defun aph/org-table-clear-row-forward ()
  "Erase contents of table cells from point to end of row.

If point is not inside an Org table, signal an error."
  (interactive)
  (unless (org-table-p)
    (error "Not in Org table"))
  (save-excursion
    (save-match-data
      (replace-regexp "[^|]" " " nil (point) (point-at-eol)))))


(provide 'aph-org-table)
