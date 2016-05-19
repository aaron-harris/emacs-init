;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; FORMS EXTENSIONS
;;;;============================================================================

;; Extensions for `forms' module.
(require 'formation)


;;; Database Query Functions
;;;=========================
(defun aph/forms-list-records (&optional name-field)
  "Return an alist mapping NAME-FIELD to record number for current db.

For use in `forms-mode'.  The returned alist has one entry per
record in the current database, with each entry of the form
\(NAME . NUM), where NAME is the value of the field whose number
is specified by NAME-FIELD and NUM is the number of the record.

If omitted, NAME-FIELD defaults to 1."
  (let ((name-field  (or name-field 1)))
    (formation-map
     (lambda ()
       `(,(nth name-field forms-fields) . ,forms--current-record)))))

(provide 'aph-forms)
