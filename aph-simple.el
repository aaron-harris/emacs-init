;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; SIMPLE EXTENSIONS
;;;;============================================================================

;; Extensions for `simple' package. 
(require 'simple)


;;; Editing Commands
;;;=================
(defun aph/open-line (n)
  "As `open-line', with support for negative argument.
An argument of -N calls `join-line' with an argument N times."
  (interactive "p")
  (if (< n 0)
      (dotimes (i (- n)) (join-line :invert))
    (open-line n)))


;;; Eval Expression
;;;================
;; This function was taken from a stackexchange answer by user Harald
;; Hanche-Olsen.  I have subsequently reformatted it to match my code
;; style.
(defvar aph/eval-expression-clean-output nil
  "If non-nil, evaluating elisp will not return extra info.

Normally, evaluating elisp (e.g., via `eval-expression' or
`ielm') will format its output with extraneous data, such as:
  (+ 1 1)
  ;=> 2 (#o2, #x2, ?\\C-b) 
This output is produced by the function
`eval-expression-print-format'.

When this variable is non-nil, all output from
`eval-expression-print-format' is silenced (so the previous
example would just return 2.")

(defun aph/eval-expression-toggle-clean-output ()
  "Toggle the variable `aph/eval-expression-clean-output'."
  (interactive)
  (setq aph/eval-expression-clean-output
        (not aph/eval-expression-clean-output)))

(defun aph/eval-expression-mute-print-format (orig-fun value)
  "Advice to enforce `aph/eval-expression-clean-output'.
This is intended as :around advice for
`eval-expression-print-format'."
  (if aph/eval-expression-clean-output
      ""
    (funcall orig-fun value)))


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


(provide 'aph-simple)
