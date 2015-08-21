;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; MPC FUNCTIONS
;;;;============================================================================

;;; This file contains functions useful for my work at the MPC,
;;; principally the geography coding I started doing around 2014.

(require 'dash)                         ; For -->


;;; Utility Functions
;;; =================
(defun aph/sum-parens (string)
  "Sum all parenthesized numbers in STRING."
  ;; The regexp command here removes everything except the numbers to sum.
  (->> (replace-regexp-in-string "[^()]*(\\([0-9]*\\))" "\\1 " string)
       split-string
       (mapcar #'string-to-int)
       (apply #'+)))


;;; Commands
;;;=========
(defun aph/sum-parens-in-region (start end)
  "Sum all parenthesized numbers in region and echo the result.
If the region is not active, sum all parenthesized numbers in
active buffer.

See `aph/sum-parens' to get similar functionality from elisp."
  (interactive "r")
  (let ((start (if (use-region-p) start (point-min)))
        (end   (if (use-region-p) end   (point-max))))
    (message "Sum of parenthesized numbers: %d"
             (aph/sum-parens (buffer-substring start end)))))

(defun aph/yank-access-inline ()
  "Yank the most recent kill, cleaning up MS Access formatting.

Specifically, collapse all whitespace in the most recent kill to
spaces, remove the first word of the kill entirely, then
yank. Also push the result back onto the kill ring (not replacing
the original).

This function is designed to clean up text copied as a rectangle
from a Microsoft Access datasheet. In these circumstances, the
cell contents are delimited by newlines and the field name is
inserted at the top, which can make doing calculations on the
data awkward."
  (interactive)
  (--> (current-kill 0)
       (split-string it)
       (cdr it)
       (mapconcat #'identity it " ") 
       (kill-new it)
       (insert-for-yank it)))


;;; IELM Functions
;;;===============
;; Functions in this section are intended for use in an IELM session.
;; Because of this, they depart from my usual convention of using
;; "aph/" as a prefix.

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
                      (mapcar #'string-to-int))))
       (cde--list)))

(defun cde (ranges)
  "Count the numbers in RANGES.

Here, RANGES may either be a comma-separated string of hyphenated
ranges, e.g. \"1-5,7,8-15\", or a list encoding the same
information, e.g., '((1 5) 7 (8 15)). For both of the examples above,
cde will return 14."
  (cond
   ((listp ranges)    (cde--list ranges))
   ((stringp ranges)  (cde--string ranges))))

(provide 'aph-mpc)
