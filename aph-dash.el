;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; DASH EXTENSIONS
;;;;============================================================================

;;; Extensions to the `dash' library.

(require 'dash)


;;; Reduce
;;;=======
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


;;; Element Search
;;;===============
(defun aph/successor-in-list (list elt &optional cycle)
  "Return the element in LIST following ELT.
If ELT is not an element of LIST, return nil.

If ELT is the last element in LIST, return (car LIST) if the
optional parameter CYCLE is non-nil; otherwise, return nil."
  (let ((found  (-drop-while (lambda (x) (not (equal x elt)))
                             list)))
    (cond
     ((null found)                   nil)
     ((or (cdr found) (null cycle))  (cadr found))
     (:else                          (car list)))))


;;; Equality Predicates
;;;====================
(defun aph/equal (&rest args)
  "As `equal', but accept any number of args.
Return t if all args are pairwise `equal', otherwise nil.
When called with fewer than two args, return t.

Transitivity of `equal' is assumed, so not all pairwise
comparisons are made."
  (or (null args)
      (-all-p (-partial #'equal (car args)) (cdr args))))


(provide 'aph-dash)
