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
