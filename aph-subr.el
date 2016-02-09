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


;;; Buffer Position Functions
;;;==========================
(defun aph/get-bol (&optional pos)
  "Return position of first character on line containing POS.
If POS is omitted, use position of point.

Do not move point."
  (save-excursion
    (let ((pos (or pos (point))))
      (goto-char pos)
      (line-beginning-position))))

(defun aph/get-eol (&optional pos)
  "Return position of last character on line containing POS.
If POS is omitted, use position of point.

Do not move point."
  (save-excursion
    (let ((pos (or pos (point))))
      (goto-char pos)
      (line-end-position))))


;;; Alist Functions
;;;================
(defun aph/assoc-delete-all (key alist)
  "As `assq-delete-all', but use `equal' rather than `eq'."
  (require 'cl-lib)                       ; For `cl-delete'
  (cl-delete key alist :test #'equal :key #'car))

(defun aph/update-alist (alist key val)
  "Update association in ALIST for KEY to VAL.
If no association exists in ALIST for KEY, add a new one at the
front of ALIST.  If multiple associations exist, only update the
first.

The return value is the new value of ALIST.  As with `delete',
this is destructive, but you should assign the result back to an
alist variable to be sure the changes are correctly made."
  (let ((elt (assoc key alist)))
    (if elt
        (setf (cdr elt) val)
      (setq alist (push `(,key . ,val) alist)))
    alist))

(defmacro aph/set-assoc (alist-var key val)
  "Update association in value of ALIST-VAR for KEY to VAL.
As `aph/update-alist', but there is no need for
back-assignment."
  `(setq ,alist-var (aph/update-alist ,alist-var ,key ,val)))


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
