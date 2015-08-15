;;; -*- lexical-binding: t -*-

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

(defvar aph/help-window-names
  '("*Help*" "*Apropos*" "*Completions*")
  "Names of buffers that `aph/quit-help-windows' should quit.")

(defun aph/quit-help-windows (&optional kill frame)
  "Quit all windows with help-like buffers.

Calls `quit-windows-on' for every buffer named in
`aph/help-windows-name'.  The optional parameters KILL and FRAME
are just as in `quit-windows-on', except FRAME defaults to t (so
that only windows on the selected frame are considered).

Note that a nil value for FRAME cannot be distinguished from an
omitted parameter and will be ignored; use some other value if
you want to quit windows on all frames."
  (interactive)
  (let ((frame (or frame t)))
    (dolist (buffer aph/help-window-names)
      (quit-windows-on buffer kill frame))))

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
;; Taken from the Yoo Box article
;; "Emacs Lisp lexical binding gotchas and related best practices" 
(defmacro aph/lexical-scope-p (var)
  "Returns t if VAR can be lexically bound, and nil otherwise.

Specially, this will return nil when called in dynamic scope, and
it will return nil if var has been declared as a special
variable (e.g., with `defvar').  All other cases should return
t."
  `(let ((,var nil)
         (f (let ((,var t)) (lambda () ,var))))
     (funcall f)))

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


;;; Sublibraries
;;;=============
(aph/require-softly 'aph-functions-dash)


(provide 'aph-functions)
