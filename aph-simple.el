;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; SIMPLE EXTENSIONS
;;;;============================================================================

;; Extensions for `simple' package.


;;; Truncate Lines
;;;===============
;; There are two problems with including `toggle-truncate-lines' in
;; mode hooks:
;; 1. I want to be explicit about whether the mode should be on or off.
;; 2. Calling `toggle-truncate-lines' spams the message log.
;; To solve the first problem, we want different named functions
;; (since these are for hooks, we don't want to use lambdas).
;;
;; To solve the second, we'll just set the `truncate-lines' variable
;; instead of calling `toggle-truncate-lines'; the remaining
;; functionality of `toggle-truncate-lines' will hopefully not be
;; needed, since usually a mode hook will be called when opening a
;; buffer.

;;;###autoload
(defun aph/truncate-lines-on ()
  "Cause current buffer to truncate long lines." 
  (setq truncate-lines t))

;;;###autoload
(defun aph/truncate-lines-off ()
  "Cause current buffer to fold long lines."
  (setq truncate-lines nil))


(provide 'aph-shr)
