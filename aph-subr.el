;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; SUBR EXTENSIONS
;;;;============================================================================

;; Extensions for `subr' module.


;;; Alist Functions
;;;================
(defun aph/assoc-delete-all (key alist)
  "As `assq-delete-all', but use `equal' rather than `eq'."
  (require 'cl-lib)                       ; For `cl-delete'
  (cl-delete key alist :test #'equal :key #'car))


(provide 'aph-subr)
