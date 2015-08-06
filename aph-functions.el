;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS
;;;;============================================================================ 


;;; Commands
;;;=========
(defun aph/delete-frame-or-exit (&optional arg)
  "Delete this frame. With only one frame, exit Emacs.

When there is more than one visible frame, run `delete-frame'.
Otherwise, exit Emacs with `save-buffers-kill-terminal'.

Any prefix ARG is passed to `save-buffers-kill-terminal' in the
single-frame case and ignored otherwise."
  (interactive "P")
  (if (> (length (visible-frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-terminal arg)))

(defun aph/kill-active-buffer (&optional choose)
  "Kill the active buffer.

With a prefix argument, choose the buffer to kill (as the
standard `kill-buffer')."
  (interactive "P")
  (if choose
      (call-interactively #'kill-buffer)
    (kill-buffer)))

(defun aph/other-window-backwards (count &optional all-frames)
  "As `other-window' but reversed."
  (interactive "p")
  (other-window (- count) all-frames))

(defun aph/ielm-get-last-output (&optional arg)
  "Return the last output produced by `ielm'.

With argument N > 0, instead return the Nth last output.
With argument N < 0, return the Nth output since last clear.
With argument N = 0, do nothing and return nil.

If N greater in absolute value than the number of uncleared
outputs in the ielm buffer, return nil."
  (let* ((arg              (or arg 1)) 
         (output-regexp    (concat "^" ielm-prompt ".+\n"
                                   "\\(?:\s.+\n\\)*"
                                   "\\([^\s].+\\)"))
        (search-function  (if (< arg 0)
                              #'search-forward-regexp
                            #'search-backward-regexp)))
    (unless (zerop arg)
      (with-current-buffer "*ielm*"
        (save-excursion 
          (goto-char (if (> arg 0) (point-max) (point-min)))
          (if (funcall search-function output-regexp nil :noerror (abs arg))
              (match-string 1)
            nil))))))

(defun aph/ielm-copy-last-output (&optional arg)
  "Copy the last output produced by `ielm' to the kill ring.

With argument N > 0, instead copy the Nth last output.
With argument N < 0, copy the Nth output since last clear.
With argument N = 0, do nothing.

Return the newly copied string, or nil if nothing was
copied (e.g., if the argument is greater than the number of
uncleared outputs)."
  (interactive "p")
  (kill-new (aph/ielm-get-last-output arg)))

(defun aph/scroll-down-by-line (&optional arg)
  "As `scroll-down-command', but ARG defaults to 1.

Also, a negative prefix argument is treated as -1, scrolling only
one line upward."
  ;; All of our changes are encapsulated in the `interactive' form.
  (interactive "^p")
  (scroll-down-command arg))

(defun aph/scroll-up-by-line (&optional arg)
  "As `scroll-up-command', but ARG defaults to 1.

Also, a negative prefix argument is treated as -1, scrolling only
one line downward."
  ;; All of our changes are encapsulated in the `interactive' form.
  (interactive "^p")
  (scroll-up-command arg))


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

(require 'cl-lib)                       ; for `cl-random'
(defun aph/weighted-comparator-maker (bias-fn)
  "Return a weighted comparator using BIAS-FN.

The returned function takes two arguments and applies BIAS-FN (a
function that takes a single argument and returns a number
indicating the relative preference for that argument) to each of
them. The difference of the results is added to 0.5, restricted
to the interval \\[0,1], and interpreted as the probability that
the function should return +1 rather than -1.

If BIAS-FN returns a non-numeric result for either argument, the
comparison is unbiased (the difference is taken to be 0.5)."
  (lambda (x y)
    (let* ((bias-x  (funcall bias-fn x))
           (bias-y  (funcall bias-fn y))
           (bias    (if (and (numberp bias-x)
                             (numberp bias-y))
                        (+ 0.5 (- bias-x bias-y))
                      0.5))
           (roll    (cl-random 1.0)))
      (if (< roll bias) +1 -1))))

;; TODO: Refactor this dependency.
(require 'dash)
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

(provide 'aph-functions) 
