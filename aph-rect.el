;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; RECTANGLE EXTENSIONS
;;;;============================================================================ 


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
