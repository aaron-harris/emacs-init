;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; MPC TESTS
;;;;============================================================================

;; Tests for the module `aph-mpc'.
(require 'aph-mpc)

(require 'aph-subr)                   ; For `aph/save-frame-excursion'


;;; MS Access Interoperability Function Tests
;;;==========================================
(defun aph/mpc-test-yank-access (fun initial expected &rest args)
  "Subroutine testing `aph/mpc-test-yank-access-*' functions.

Call `aph/yank-command-on-string' on string INITIAL, command FUN,
and ARGS.  Return t if all of these requirements are met:
- The return value of the call is EXPECTED.
- After the call, the top of the kill ring is EXPECTED.
- The second entry of the kill ring is INITIAL.
If any requirement is not met, signal an error with `should'.

Do not change the kill ring or current buffer."
  (require 'aph-dash)                   ; For `aph/equal'
  (let ((kill-ring nil))
    (should (aph/equal expected
                       (apply #'aph/yank-command-on-string initial fun args)
                       (pop kill-ring)))
    (should (equal initial (pop kill-ring)))))

(ert-deftest aph/mpc-test-yank-access-inline ()
  "Test `aph/mpc-yank-access-inline'."
  (require 'aph-dash)                   ; For `aph/equal'
  (aph/mpc-test-yank-access #'aph/mpc-yank-access-inline
                            "foo bar\nbaz"
                            "bar baz"))

(ert-deftest aph/mpc-test-yank-access-overfull ()
  "Test `aph/mpc-yank-access-overfull'."
  (require 'aph-dash)                  ; For `aph/equal'
  (aph/mpc-test-yank-access #'aph/mpc-yank-access-overfull
                            "57\t35 foo 75 -3"
                            "0 0 25 0")
  (aph/mpc-test-yank-access #'aph/mpc-yank-access-overfull
                            "57\t35 foo 75 -3"
                            "25 0 65 0"
                            10))


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
