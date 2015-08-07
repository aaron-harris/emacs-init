;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS (DASH-DEPENDENT)
;;;;============================================================================

;;; The functions in this file are dependent on the dash library, and
;;; are kept here so that initialization can fail gracefully when dash
;;; is not available.

(require 'dash) 


;;; Utility Functions
;;;==================
(defun aph/reduce (fn seq)
  "As `-reduce', but accept all sequences, not just lists."
  (if (listp seq)
      (-reduce fn seq)
    (-reduce fn (append seq nil))))

(defun aph/reductions (fn seq)
  "As `aph/reduce', but return intermediate results.
These results are returned as a list."
  (->> seq
       (aph/reduce (lambda (acc val) 
                     (let ((acc (if (listp acc) acc (list acc))))
                       (cons (funcall fn (car acc) val) acc))))
       (reverse)))

(provide 'aph-functions-dash)
