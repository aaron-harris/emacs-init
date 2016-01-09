;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; MPC TESTS
;;;;============================================================================

;; Tests for the module `aph-mpc'.
(require 'aph-mpc)

(require 'aph-subr)                   ; For `aph/save-frame-excursion'


;;; Calc Bar Tests
;;;===============
(defun aph/mpc-test-calc-bar-frame (frame)
  "Return t if FRAME is set up as `aph/mpc-calc-bar'.
If it is not, signal an error.  This error is the same as that
signaled by the `should' macro, so the calling test should behave
as if the `should' were inlined.

This function is called as a subroutine by ERT tests for
`aph/mpc-calc-bar'."
  ;; Name and contents
  (should (equal "Calc Bar" (frame-parameter frame 'name)))
  (let* ((win1 (frame-first-window frame))
         (win2 (window-in-direction 'right win1)))
    (should (equal "*Calculator*" (buffer-name (window-buffer win1))))
    (should (equal "*ielm*" (buffer-name (window-buffer win2)))))
  ;; Dimensions and positioning
  (should (= (frame-pixel-height frame)
             aph/mpc-calc-bar-height))
  (should (= (aph/frame-true-width frame)
             (aph/frame-fullscreen-width))) 
  ;; The frame parameters 'left and 'top are sometimes recalculated as
  ;; absolute values (e.g., '(+ 5) becomes 5).  Instead of checking
  ;; the parameter directly, then, we set the parameter to what we
  ;; think it already is and see if the value changes.
  (let ((x-pos (frame-parameter frame 'left))
        (y-pos (frame-parameter frame 'top)))
    (set-frame-parameter frame 'left `(+ ,aph/frame-offset))
    (should (equal x-pos (frame-parameter frame 'left)))
    (set-frame-parameter frame 'top `(- ,aph/frame-w32-taskbar-height))
    (should (equal y-pos (frame-parameter frame 'top)))))

(ert-deftest aph/mpc-test-calc-bar-no-arg ()
  "Test `aph/mpc-calc-bar' without prefix arg."
  (aph/save-frame-excursion
   (let* ((frame (selected-frame))
          (fname (frame-parameter frame 'name)))
     (should (eq frame (aph/mpc-calc-bar)))
     (aph/mpc-test-calc-bar-frame frame)
     (set-frame-parameter frame 'name fname))))

(ert-deftest aph/mpc-test-calc-bar-with-arg ()
  "Test `aph/mpc-calc-bar' with prefix arg."
  (aph/save-frame-excursion
    (let ((num-frames (length (frame-list)))
          (frame      (aph/mpc-calc-bar :new-frame)))
      (should (= (1+ num-frames) (length (frame-list))))
      (aph/mpc-test-calc-bar-frame frame))))


(provide 'aph-mpc-test)
