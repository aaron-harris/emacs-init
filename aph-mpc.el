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

(provide 'aph-mpc)
