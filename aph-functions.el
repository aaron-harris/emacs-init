;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS
;;;;============================================================================ 


;;; Macros
;;;=======
(defmacro aph/with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))


;;; Subroutines
;;; ===========
(defun aph/sum-parens (string)
  "Sum all parenthesized numbers in STRING."
  ;; The regexp command here removes everything except the numbers to sum.
  (->> (replace-regexp-in-string "[^()]*(\\([0-9]*\\))" "\\1 " string)
       split-string
       (mapcar #'string-to-int)
       (apply #'+)))


;;; Commands
;;;=========
(defun aph/apropos-function (pattern &optional commands-only var-predicate)
  "As `apropos-command', but show all functions by default.
The behavior of a prefix argument is inverted, so supplying a
prefix argument will show only commands (and override a non-nil
value for `apropos-do-all')"
  (interactive (list (apropos-read-pattern
		      (if current-prefix-arg
			  "command" "command or function"))
		     current-prefix-arg))
  (apropos-command pattern (not commands-only) var-predicate))

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

(defun aph/kp-enter-newline-toggle (&optional verbose)
  "Toggle whether <kp-enter> should act like C-n instead of enter.
Accomplish this by updating the entry for <kp-enter> in
`function-key-map'.  If this entry is something other than these
two keys, restore it to the default mapping (enter, i.e. C-m).

When called interactively, or when the optional parameter is
supplied, supply feedback messages. Always print a message when
the entry being overridden is unexpected.

For convenience, return the new mapping."
  (interactive "p")
  (let ((current-def  (lookup-key function-key-map (kbd "<kp-enter>")))
        (new-def      nil)
        (msg      nil))
    ;; The keymap uses [13] for C-m, so we'll respect its conventions
    ;; and use [14] for C-n.
    (cond
     ((equal current-def [13])
      (setq new-def [14]
            msg      "Decoupling <kp-enter> for motion"))
     ((equal current-def [14])
      (setq new-def  [13]
            msg      "Recoupling <kp-enter> to <enter>"))
     (t
      (setq new-def [13]
            verbose  t
            msg      "Restoring <kp-enter> to default (was %S)")))
    
    (define-key function-key-map (kbd "<kp-enter>") new-def)
    (when verbose (message msg current-def))
    new-def))

(defun aph/other-window-backwards (count &optional all-frames)
  "As `other-window' but reversed."
  (interactive "p")
  (other-window (- count) all-frames))

(defvar aph/help-window-names
  '("*Help*" "*Apropos*" "*Completions*" "*disabled command*"
    "*Command History*")
  "Names of buffers that `aph/quit-help-windows' should quit.")

(defun aph/quit-help-windows (&optional kill frame)
  "Quit all windows with help-like buffers.

Call `quit-windows-on' for every buffer named in
`aph/help-windows-name'.  The optional parameters KILL and FRAME
are just as in `quit-windows-on', except FRAME defaults to t (so
that only windows on the selected frame are considered).

Note that a nil value for FRAME cannot be distinguished from an
omitted parameter and will be ignored; use some other value if
you want to quit windows on all frames."
  (interactive)
  (let ((frame (or frame t)))
    (dolist (buffer aph/help-window-names)
      (ignore-errors
        (quit-windows-on buffer kill frame)))))

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

(defun aph/newline (n)
  "As `newline', with support for negative argument.
An argument of -N calls `join-line' N times."
  (interactive "p")
  (if (< n 0)
      (dotimes (i (- n)) (join-line))
    (newline n)))

(defun aph/open-line (n)
  "As `open-line', with support for negative argument.
An argument of -N calls `join-line' with an argument N times."
  (interactive "p")
  (if (< n 0)
      (dotimes (i (- n)) (join-line :invert))
    (open-line n)))

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

(defun aph/sum-parens-in-region (start end)
  "Sum all parenthesized numbers in region and echo the result.
If the region is not active, sum all parenthesized numbers in
active buffer.

See `aph/sum-parens' to get similar functionality from elisp."
  (interactive "r")
  (let ((start (if (use-region-p) start (point-min)))
        (end   (if (use-region-p) end   (point-max))))
    (message "Sum of parenthesized numbers in %s: %d"
             (if (use-region-p) "region" "buffer")
             (aph/sum-parens (buffer-substring start end)))))


;;; Utility Functions
;;;==================
(require 'cl-lib)                       ; For `cl-delete'
(defun aph/assoc-delete-all (key alist)
  "As `assq-delete-all', but use `equal' rather than `eq'."
  (cl-delete key alist :test #'equal :key #'car))

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


;;; Sublibraries
;;;=============
(aph/require-softly 'aph-functions-dash)


(provide 'aph-functions)
