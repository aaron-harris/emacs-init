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


;;; Random Record Selection
;;;========================
(defun aph/forms-random-record ()
  "Go to a randomly selected record in current database."
  (interactive)
  (forms-jump-record (1+ (random forms--total-records))))

(defvar-local aph/forms-random-record-weight nil
  "Field number to use for `aph/forms-random-record-weighted'.")

(defvar-local aph/forms-random-record-weight-transform
  #'aph/string-to-natural
  "Transformation for `aph/forms-random-record-weight'.
This function is applied by `aph/forms-random-record-weighted' to
transform the weighting factor from a string to an integer.

The default value, `aph/string-to-natural', is just a
concatenation of the standard functions `string-to-number',
`abs', and `truncate'.  This ensures that the values returned
will at least be non-negative integers, as otherwise the behavior
of `aph/forms-random-record-weighted' has not been established.")

(defun aph/forms-random-record-weighted ()
  "As `aph/forms-random-record', but die is weighted.

Interpret the field specified by the variable
`aph/forms-random-record-weight' as a weighting factor, using the
value of `aph/forms-random-record-weight-transform' to transform
this into an integer.

The chance of selecting any particular record R is then n/N,
where n is the value R has for the weighting field and N is the
total of this field across all records in the database."
  (interactive)
  (let* ((counter (lambda (acc) 
                    (+ acc (funcall aph/forms-random-record-weight-transform
                                    (nth aph/forms-random-record-weight
                                         forms-fields)))))
         (total   (formation-reduce counter 0))
         (roll    (random total))) 
    (catch 'found
      (formation-reduce
       (lambda (acc)
         (setq acc (funcall counter acc))
         (if (< roll acc)
             (throw 'found forms--current-record)
           acc))
       0))))

(defun aph/forms-random-record-maybe-weighted ()
  "Go to random record in this db, using weight if available.
If `aph/forms-random-record-weight' is non-nil, call
`aph/forms-random-record-weighted'.  Otherwise, call
`aph/forms-random-record'."
  (interactive)
  (call-interactively (if aph/forms-random-record-weight
                          #'aph/forms-random-record-weighted
                        #'aph/forms-random-record)))

;; Some other useful transforms
(defun aph/forms-rating-transform (string)
  "Transform a half-point rating into an integer weight.
The transformation is linear (i.e., the numbers are just
doubled).

This function is suitable for use in the variable
`aph/forms-random-record-weight-transform'."
  (truncate (* 2 (string-to-number string))))


(provide 'aph-forms)
