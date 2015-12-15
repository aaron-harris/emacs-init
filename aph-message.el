;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; MESSAGE EXTENSIONS
;;;;============================================================================

;; Functions extending the `message' function.


;;; Basic functions
;;;================
(defun aph/canary (&rest args)
  "Print a message containing ARGS."
  (message "Canary called with args %s" args))


(provide 'aph-message)
