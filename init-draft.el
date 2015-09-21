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
(defun aph/slide-buffer-forward ()
  "Slide active buffer to next window.
Display this buffer in the next window forward (in the same
ordering as `other-window') that is not dedicated to its current
buffer, and display in this window the previous buffer displayed
here (using `switch-to-prev-buffer')."
  (interactive)
  (let ((buf  (current-buffer))
        (win  (get-window-with-predicate
               (lambda (win) (not (window-dedicated-p win))))))
    (when win
      (set-window-buffer win buf)
      (switch-to-prev-buffer))))
