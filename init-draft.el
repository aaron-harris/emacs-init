;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; DRAFT AREA
;;;;============================================================================

;; This file is for drafting new portions of the init files. It is not
;; automatically loaded on initialization.



;;; `hippie-unexpand'
;;;==================
(define-key read-expression-map [(tab)] 'hippie-expand)

(defun hippie-unexpand ()
  (interactive)
  (hippie-expand -1))

(define-key read-expression-map [(shift tab)] 'hippie-unexpand)


;;; Sliding door commands
(defun aph/get-nth-window-with-predicate
    (n predicate &optional minibuf all-frames default)
  "As `get-window-with-predicate', but return the Nth success."
  ;; TODO: Write this function.
  )

(defun aph/slide-buffer-forward (&optional count)
  "Slide active buffer to another window.

Display this buffer N windows forward (in the same ordering as
`other-window'), skipping windows dedicated to their current
buffers, and display in this window the previous buffer displayed
here (using `switch-to-prev-buffer')."
  (interactive "p")
  (let ((buf  (current-buffer))
        (win  (aph/get-nth-window-with-predicate
               n (lambda (win) (not (window-dedicated-p win))))))
    (when win 
      (set-window-buffer win buf)
      (switch-to-prev-buffer))))
