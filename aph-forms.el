;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; FORMS EXTENSIONS
;;;;============================================================================

;; Extensions for `forms' module.
(require 'forms)


;;; Additional Hooks
;;;=================
(defvar aph/forms-change-record-hook nil
  "Hook run after changing records in `forms-mode'.")

(defun aph/forms-change-record-hook-run (&rest args)
  "Run `aph/forms-change-record-hook'.

The ARGS are ignored.  The reason for including them is so that
this function can be used as advice.  By default this function is
installed as :after advice on `forms-jump-record'."
  (run-hooks 'aph/forms-change-record-hook))

(advice-add #'forms-jump-record :after #'aph/forms-change-record-hook-run)


;;; Functional Subroutines
;;;=======================
(defun aph/forms-reduce (fun &optional acc)
  "Reduce FUN over all records in current database.
Use ACC as the initial value for the accumulator, or nil if ACC
is not provided.

The function FUN should take a single argument (the accumulator)
and return a value of the same type.  The data in the current
record can be accessed using dynamic variables such as
`forms-fields'.

Do not change current record (or, more accurately, save current
record and restore it after completion).  Since this is
frequently necessary to refresh some aspect of the buffer's
appearance, it is run even in the event of an error or nonlocal
exit.

Run `aph/forms-change-record-hook' only once, when restoring the
initial record."
  (save-excursion
    (unwind-protect
        (let ((aph/forms-change-record-hook nil)) 
          (forms-first-record)
          (while (< forms--current-record forms--total-records)
            (setq acc (funcall fun acc))
            (forms-next-record 1))
          (funcall fun acc)
          acc)
      (run-hooks 'aph/forms-change-record-hook))))


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
    (aph/forms-reduce
     (lambda (acc)
       (cons `(,(nth name-field forms-fields) . ,forms--current-record)
             acc)))))


;;; Auxiliary File
;;;===============
(defvar-local aph/forms-aux-buffer nil
  "The auxiliary file buffer for the current database.")

(defun aph/forms-open-auxiliary-file-from-field (&optional field)
  "Open the file named in FIELD in a separate window.
Kill any other buffer previously created by this function,
without saving any changes.

Return the newly opened buffer, but do not select it.

If omitted, FIELD defaults to 1."
  (interactive)
  (let* ((field       (or field 1))
         (filename    (nth field forms-fields))
         (window      (selected-window))
         (aux-buffer  aph/forms-aux-buffer))
    (when aux-buffer  (aph/kill-buffer-nowarn aux-buffer)) 
    (setq aux-buffer  (find-file-other-window filename))
    (select-window window)
    ;; The circumlocution here is because `aph/forms-aux-buffer' is
    ;; buffer-local, and we want to set it in our original buffer, not
    ;; in the auxiliary buffer itself.
    (setq aph/forms-aux-buffer aux-buffer)))


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
         (total   (aph/forms-reduce counter 0))
         (roll    (random total))) 
    (catch 'found
      (aph/forms-reduce
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
