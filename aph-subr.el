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
