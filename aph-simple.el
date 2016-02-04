;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; SIMPLE EXTENSIONS
;;;;============================================================================

;; Extensions for `simple' package.
(require 'simple)

(require 'hydra)


;;; Newline Commands
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


;;; Yank Commands
;;;==============
(defun aph/yank-command-on-string (string command &rest args)
  "Return text inserted by COMMAND when STRING is most recent kill.
Pass any ARGS to COMMAND.

Do this in a temporary buffer, so that current buffer is not
changed or modified.  (Of course, if COMMAND selects a buffer
itself, this guarantee cannot be enforced.)

Presumably COMMAND is a function that yanks text from the kill
ring (possibly with some other processing involved).  This
function effectively transforms that command into a string
processing command, although `kill-ring' is still changed as a
side-effect (by the addition of STRING, plus whatever changes are
made by COMMAND)."
  (with-temp-buffer
    (kill-new string)
    (apply command args)
    (buffer-string)))


;;; Motion Commands
;;;================
(defun aph/move-beginning-of-line (&optional arg)
  "Combine `move-beginning-of-line' and `back-to-indentation'.

Behave as `move-beginning-of-line', unless point is already at
beginning of line, in which case call `back-to-indentation'.

If ARG is supplied, then it is interpreted as in
`move-beginning-of-line' and `back-to-indentation' is not
called.

Return the new value of point."
  (interactive "^P")
  (cond
   (arg     (move-beginning-of-line (prefix-numeric-value arg)))
   ((bolp)  (back-to-indentation))
   (:else   (move-beginning-of-line 1)))
  (point))


;;; Capitalization Commands
;;;========================
(defhydra aph/hydra-caps (:color red)
  "Capitalization"
  ;; Capitalization commands
  ("c"   capitalize-word "proper")
  ("l"   downcase-word   "lower")
  ("u"   upcase-word     "upper")
  ;; Movement commands
  ("b"   backward-word   "back")
  ("f"   forward-word    "forward")
  ;; Aliases to allow use with or without meta
  ("M-c" capitalize-word nil)
  ("M-l" downcase-word   nil)
  ("M-u" upcase-word     nil)
  ("M-b" backward-word   nil)
  ("M-f" forward-word    nil)
  ;; Quit
  ("q"   nil             "quit" :color blue))


(provide 'aph-simple)
