;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ORG MODE AGENDA SUBROUTINES
;;;;============================================================================

;;; This file contains functions which are useful in building custom
;;; agenda commands.

;; Some functions in this file require the 'aph-comparators library at runtime.


;;; Basic Extensions
;;;=================
(defun aph/org-agenda-redo ()
  "As `org-agenda-redo' with prefix arg.

This is exactly the command bound by default to g in
`org-agenda-mode', except it's not a lambda."
  (interactive)
  (org-agenda-redo t))

(provide 'aph-org-agenda)
