;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS
;;;;============================================================================ 


;;; Commands
;;;=========
(defun aph/kill-active-buffer (&optional choose)
  "Kill the active buffer.

With a prefix argument, choose the buffer to kill (as the
standard `kill-buffer')."
  (interactive "P")
  (if choose
      (call-interactively #'kill-buffer)
    (kill-buffer))) 


;;; Utility Functions
;;;==================
(defun aph/random-comparator (&rest args)
  "Randomly return +1 or -1.

Randomly return +1 with probability 1/2 and -1 with
probability 1/2. Ignore any ARGS.

Intended for use as a comparator in a sorting mechanism. When
used in such a way, the results will be shuffled (sorted
randomly)."
  (truncate (* 2 (- (random 2) 0.5)))) 

;; TODO: Refactor this `fboundp' test.
(if (fboundp #'-reduce)
    (progn
      (defun aph/reduce (fn seq)
        "As `-reduce', but accepts all sequences, not just lists."
        (if (listp seq)
            (-reduce fn seq)
          (-reduce fn (append seq nil))))

      (defun aph/reductions (fn seq)
        "As `aph/reduce', but returns intermediate results.
These results are returned as a list."
        (->> seq
             (aph/reduce (lambda (acc val)
                           (let ((acc (if (listp acc) acc (list acc))))
                             (cons (funcall fn (car acc) val) acc))))
             (reverse))))

  (message "Function `aph/reduce' not defined: `dash' is not loaded."))

(provide 'aph-functions) 
