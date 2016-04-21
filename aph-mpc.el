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


;;; IELM Functions
;;;===============
;; Functions in this section are intended for use in an IELM session.
;; Because of this, they depart from my usual convention of using
;; "aph/" as a prefix.

(defvar cde-page-size 50
  "The increment size for alphabetic characters in `cde'.
See the documentation of `cde' for more information.")

(defun cde--unpage (ref)
  "Convert REF to an integer using `cde-page-size'.
Here REF should be a string of the form \"nX\", where n is an
integer and X is any string.

The return value is n unless X is the string \"B\", in which case
it is n plus the value of `cde-page-size'.

This function is used as a subroutine by `cde'."
  (save-match-data
    (string-match "\\([0-9]+\\)\\(B?\\)" ref)
    (let ((n  (string-to-int (match-string 1 ref)))
          (x  (match-string 2 ref)))
      (+ n (if (equal x "B") cde-page-size 0)))))

(defun cde--list (ranges)
  "Count the numbers in RANGES.

As `cde', but RANGES must be in list form."
  (->> ranges
       (mapcar (lambda (elt)
                 (if (and (consp elt) (cdr elt))
                     (- (cadr elt) (1- (car elt)))
                   1)))
       (apply #'+)))

(defun cde--string (ranges)
  "Count the numbers in RANGES.

As `cde', but RANGES must be in string form."
  (->> (split-string ranges ",")
       (mapcar (lambda (range)
                 (->> (split-string range "-") 
                      (mapcar #'cde--unpage))))
       (cde--list)))

;;;###autoload
(defun cde (ranges)
  "Count the numbers in RANGES.

Here, RANGES may either be a comma-separated string of hyphenated
ranges, e.g. \"1-5,7,8-15\", or a list encoding the same
information, e.g., '((1 5) 7 (8 15)). For both of the examples above,
cde will return 14.

Additionally, the alphabetic character \"A\" and \"B\" are
interpreted as sides of a page, with the page size given by
`cde-page-size'.  Thus if `cde-page-size' is 50 (the default),
then the string \"5A\" will be interpreted as the integer 5,
while the string \"5B\" will be interpreted as 55.  This feature
is unavailable if RANGES is presented as a list."
  (cond
   ((listp ranges)    (cde--list ranges))
   ((stringp ranges)  (cde--string ranges))))

;;;###autoload
(defun cde-format (&optional verbose)
  "Convert word at point with `cde'.

If the text at point looks like a suitable input for `cde',
replace it with a string of the form

    N (RANGES)

where RANGES is the original word and N is the result of calling
`cde' on RANGES.  The return value is the text inserted.

If the text at point is not suitable for `cde', do nothing and
return nil.  Interactively, or with VERBOSE non-nil, print an
explanatory message."
  (interactive "p")
  (require 'dash)
  (-let ((ranges         (thing-at-point 'symbol))
         ((start . end)  (bounds-of-thing-at-point 'symbol)))
    (cond
     ;; Format if possible
     ((string-match-p "^\\([0-9]+[AB]?[,-]?\\)+" ranges)
      (setf (buffer-substring start end)
            (format "%d (%s)" (cde ranges) ranges)))
     ;; Display message if desired
     (verbose
      (message "Cannot use `cde' at point")
      nil))))


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
