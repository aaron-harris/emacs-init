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


;;; Auxiliary File
;;;===============
(let ((aux-buffer nil))
  (defun aph/forms-open-auxiliary-file-from-field (&optional field)
    "Open the file named in FIELD in a separate window.
Kill any other buffer previously created by this function,
without saving any changes.

Return the newly opened buffer, but do not select it.

If omitted, FIELD defaults to 1."
    (interactive)
    (let* ((field     (or field 1))
           (filename  (nth field forms-fields))
           (window    (selected-window)))
      (when aux-buffer (aph/kill-buffer-nowarn aux-buffer))
      (setq aux-buffer (find-file-other-window filename))
      (select-window window)
      aux-buffer)))


(provide 'aph-forms)
