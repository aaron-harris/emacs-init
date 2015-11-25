;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; RECTANGLE EXTENSIONS
;;;;============================================================================ 

;; Extensions for `rect' module.


;;; Rectangle Commands
;;;===================
(defun aph/yank-rectangle-from-kill-ring (&optional arg verbose)
  "Yank the top of kill ring as a rectangle.
Make the \"last killed rectangle\" be the top entry of the kill
ring, then yank that rectangle at point.

With \\[universal-argument] as argument, just save the top entry
of the kill ring as a rectangle, without yanking.  Print a
message to that effect.  When called from elisp, this message is
suppressed unless the optional argument VERBOSE is supplied.
 
With argument N, save the Nth most recent kill instead of the
most recent."
  (interactive "P\np")
  (let ((n          (if (numberp arg) arg 1))
        (save-only  (and arg (listp arg)))
        (width      0))
    (with-temp-buffer
      (yank n)
      ;; Scan for maximum line width
      (dotimes (i (point-max))
        (goto-char (1+ i))
        (setq width (max width (current-column))))
      ;; Pad final line to max width
      (while (< (current-column) width)
        (insert " ")) 
      (copy-rectangle-as-kill 1 (point-max)))
    (if save-only
        (when verbose (message "Most recent kill saved as rectangle."))
      (yank-rectangle))))


;;; Emacs 25 Rectangle Motion Patch
;;;================================
;; There seems to be a bug in Emacs 25 where movement commands in
;; rectangle mode hang when given a prefix argument.  This is a
;; workaround until the issue is fixed.
(defun aph/rectangle-repetition-fix (fn cmd n &optional other-cmd)
  "Advice to fix bug in `rectangle-mark-mode' motion commands.

The basic cursor motion commands in `rectangle-mark-mode' that
were introduced in Emacs 25 do not currently handle their prefix
arguments correctly.  These commands (principally
`rectangle-forward-char' and `rectangle-backward-char') delegate
to `rectangle--*-char'.  This function fixes the problem when
used as advice :around `rectangle--*-char'."
  (cond
   ((< n 0)  (aph/rectangle-repetition-fix fn other-cmd (- n)))
   ((= n 0)  (funcall fn cmd 0 other-cmd))
   ((> n 0)  (dotimes (i (1- n) (funcall fn cmd 1 other-cmd))
               (funcall fn cmd 1 other-cmd)))))


(provide 'aph-rect)
