;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; SILENCE TESTS
;;;;============================================================================

;; Tests for the module aph-silence.el.
(require 'aph-silence)


;;; Tests
;;;======
(ert-deftest aph/silence-test-predicate ()
  "Test `aph/silence-message-p'." 
  (let ((aph/silence-list    nil)
        (aph/silence-enabled t)
        flag)
    ;; Test empty list
    (should (not (aph/silence-message-p "foo")))
    ;; Test against regexp
    (push "foo.*" aph/silence-list) 
    (should (aph/silence-message-p "foo"))
    (should (aph/silence-message-p "foobar"))
    (should (aph/silence-message-p "barfoo"))
    (should-not (aph/silence-message-p "bar"))
    (should-not (aph/silence-message-p "barrel of monkeys"))
    ;; Test against function 
    (push (lambda (msg) (> (length msg) 8)) aph/silence-list)
    (should (aph/silence-message-p "barrel of monkeys"))
    ;; Respect `aph/silence-enabled'
    (let ((aph/silence-enabled nil))
      (should-not (aph/silence-message-p "foo")))
    ;; Short-circuit when possible
    (push (lambda (msg) (setq flag t) nil) aph/silence-list)
    (push "foo" aph/silence-list)
    (should (aph/silence-message-p "foo"))
    (should (null flag)) 
    ;; Signal error for invalid entry
    (push 7 aph/silence-list)
    (should-error (aph/silence-message-p "foo")) 
    (setf (car aph/silence-list) (list (make-symbol "foo")))
    (should-error (aph/silence-message-p "foo"))))

(ert-deftest aph/silence-test-advice ()
  "Test the function `aph/silence-advice'."
  (require 'aph-advice)                 ; For `aph/with-advice'
  (aph/with-advice ((:genname #'message :around #'aph/silence-advice))
    (let ((aph/silence-list '("foo"))
          (aph/silence-enabled t))
      (should (equal (message "bar") (current-message)))
      (should-not (equal (message "foo") (current-message)))
      (should (equal (message "%s" "foo") "foo"))
      (let ((aph/silence-enabled nil))
        (should (equal (message "foo") (current-message))))
      (let ((aph/silence-list nil))
        (should (equal (message "foobar") (current-message)))))))

      
(provide 'aph-advice-test)
