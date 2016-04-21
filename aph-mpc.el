;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; MPC FUNCTIONS
;;;;============================================================================

;;; This file contains functions useful for my work at the MPC,
;;; principally the geography coding I started doing around 2014.

(require 'dash)                         ; For -->, ->>, etc.


;;; MS Access Interoperability
;;;===========================
(defun aph/mpc-yank-access-inline (&optional transform)
  "Yank the most recent kill, cleaning up MS Access formatting.

Specifically, collapse all whitespace in the most recent kill to
spaces, remove the first word of the kill entirely, then yank.
Also push the result back onto the kill ring (not replacing the
original).

If the optional parameter TRANSFORM is supplied, it should be a
function taking a string and returning a string.  This function
is applied to each word of the kill (other than the first, which
is still removed), in the way of `map'.

This function is designed to clean up text copied as a rectangle
from a Microsoft Access datasheet.  In these circumstances, the
cell contents are delimited by newlines and the field name is
inserted at the top, which can make doing calculations on the
data awkward."
  (interactive)
  (let ((transform (or transform #'identity)))
    (--> (current-kill 0)
         (split-string it)
         (cdr it)
         (mapconcat transform it " ") 
         (kill-new it)
         (insert-for-yank it))))

(defvar aph/mpc-yank-access-overfull-default-threshold 50
  "The default arg for `aph/mpc-yank-access-overfull'.
This should be a number.")

(defun aph/mpc-yank-access-overfull (&optional threshold)
  "As `aph/mpc-yank-access-inline', subtracting THRESHOLD.

This function supplies `aph/mpc-yank-access-inline' with a custom
transform.  That transform interprets each word in the kill as a
number N (treating non-numbers as zero) and returns a string
containing the number (max 0 (- N THRESHOLD)).

THRESHOLD can be supplied by a numeric prefix argument.  If
THRESHOLD is nil or omitted, it defaults to the value of
`aph/mpc-yank-access-overfull-default-threshold'."
  (interactive "P")
  (let ((threshold (if threshold (prefix-numeric-value threshold)
                     aph/mpc-yank-access-overfull-default-threshold)))
    (aph/mpc-yank-access-inline
     (lambda (n)
       (-> n
           string-to-number
           (- threshold)
           (max 0)
           number-to-string)))))


;;; Calc Bar
;;;=========
(defvar aph/mpc-calc-bar-height 143
  "The height in pixels for `aph/mpc-calc-bar'.")

(defun aph/mpc-calc-bar--geometry (frame)
  "Set FRAME's size and position for `aph/mpc-calc-bar'.
The buffers displayed in FRAME are not affected."
  (require 'aph-frame)
  (w32-send-sys-command #xf120 frame)   ; De-maximize frame
  (set-frame-parameter frame 'left `(+ ,aph/frame-offset)) 
  (set-frame-width frame (aph/frame-fullscreen-width) (not :pretend) :pixels)
  (set-frame-height frame aph/mpc-calc-bar-height (not :pretend) :pixels)
  (set-frame-parameter frame 'top `(- ,aph/frame-w32-taskbar-height)))

(defun aph/mpc-calc-bar--buffers (frame)
  "Set the buffers displayed in FRAME for `aph/mpc-calc-bar'.
The size and position of FRAME are not affected.

Note that this function selects FRAME as a side effect."
  (select-frame frame)
  (calc)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (ielm)
  (other-window -1))

(defun aph/mpc-calc-bar (&optional new-frame)
  "Set up the current frame as a \"calc bar\".
This is a short, full-width frame with two windows.  The
left-hand window should have a `calc' buffer and the right an
`ielm' buffer.

With a prefix argument, open the calc bar as a new frame.

In either case, the return value is the calc bar frame."
  (interactive "P")
  (let* ((params '((name          . "Calc Bar")
                   (maximized     . nil)
                   (user-size     . t)
                   (user-position . t)))
         (frame  (if new-frame (make-frame params)
                   (modify-frame-parameters nil params)
                   (selected-frame))))
    (aph/mpc-calc-bar--geometry frame)
    (aph/mpc-calc-bar--buffers frame)
    frame))

(provide 'aph-mpc)
