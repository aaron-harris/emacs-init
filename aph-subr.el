;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; SUBR EXTENSIONS
;;;;============================================================================

;; Extensions for `subr' module.


;;; String Conversion Functions
;;;============================
(defun aph/string-to-integer (string)
  "Composition of `string-to-number' and `truncate'."
  (truncate (string-to-number string)))

(defun aph/string-to-natural (string)
  "Composition of `string-to-number', `abs', `truncate'."
  (truncate (abs (string-to-number string))))


;;; Alist Functions
;;;================
(defun aph/assoc-delete-all (key alist)
  "As `assq-delete-all', but use `equal' rather than `eq'."
  (require 'cl-lib)                       ; For `cl-delete'
  (cl-delete key alist :test #'equal :key #'car))

(defmacro aph/assoc-delete-in (alist-place key)
  "Delete association in value of ALIST-PLACE for KEY.
As `aph/assoc-delete-all', but there is no need for
back-assignment."
  (declare (debug (gv-place form)))
  `(setf ,alist-place (aph/assoc-delete-all ,key ,alist-place)))

(defmacro aph/assq-delete-in (alist-place key)
  "As `aph/assoc-delete-in', but use `assq-delete-all'."
  (declare (debug (gv-place form)))
  `(setf ,alist-place (assq-delete-all ,key ,alist-place)))

(defun aph/update-alist (alist key val &optional fun)
  "Update association in ALIST for KEY to VAL.
If no association exists in ALIST for KEY, add a new one at the
front of ALIST.  If multiple associations exist, only update the
first.

If the optional parameter FUN is supplied, it is used in place of
`assoc' to find the association for KEY.  Thus one may use
`assq', `cl-assoc', etc.

The return value is the new value of ALIST.  As with `delete',
this is destructive, but you should assign the result back to an
alist variable to be sure the changes are correctly made."
  (let* ((fun  (or fun #'assoc))
         (elt  (funcall fun key alist)))
    (if elt
        (setf (cdr elt) val)
      (setq alist (push `(,key . ,val) alist)))
    alist))

(defmacro aph/set-assoc (alist-place key val)
  "Update association in value of ALIST-PLACE for KEY to VAL.
As `aph/update-alist', but there is no need for
back-assignment, and FUN is always `assoc'.

Note that ALIST-PLACE may be any generalized variable containing an
alist, not just a symbol whose value is an alist."
  (declare (debug (gv-place form form)))
  `(setf ,alist-place (aph/update-alist ,alist-place ,key ,val)))

(defmacro aph/set-assq (alist-place key val)
  "As `aph/set-assoc', but use `assq'."
  (declare (debug (gv-place form form)))
  `(setf ,alist-place (aph/update-alist ,alist-place ,key ,val #'assq)))


;;; Cryptographic Hash Functions
;;;=============================
(defun aph/md5-insert (string)
  "Insert the MD5 hash of STRING into current buffer.
Interactively, prompt for STRING."
  (interactive "sString to hash: ")
  (insert (md5 string)))


;;; Excursions
;;; ==========
(defmacro aph/save-frame-excursion (&rest body)
  "As `save-window-excursion', but for frame configurations."
  (declare (indent 0) (debug t))
  (let ((frame-config (make-symbol "frame-config")))
    `(let ((,frame-config (current-frame-configuration)))
       (unwind-protect (progn ,@body)
         (set-frame-configuration ,frame-config)))))


(provide 'aph-subr)
