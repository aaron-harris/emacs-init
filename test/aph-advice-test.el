;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; ADVICE TESTS
;;;;============================================================================

;; Tests for the module aph-advice.el.
(require 'aph-advice)


;;; Tests for `aph/advice-genname'
;;;===============================
(ert-deftest aph/advice-test-genname ()
  "Test functionality of `aph/advice-genname'."
  (require 'dash)
  (let* ((results  (mapcar #'aph/advice-genname
                           '(nil
                             ((foo . bar))
                             ((name . fred))
                             ((foo . bar) (name . fred)))))
         (names    (mapcar (apply-partially #'assoc-default 'name) results))
         (strings  (mapcar #'symbol-name names)))
    (should (-every-p #'identity names))
    (should (-every-p #'symbolp names))
    (should (-none-p #'intern-soft names))
    (should (-every-p (apply-partially #'string-prefix-p "aph/advice:")
                      (-take 2 strings)))
    (should (-every-p (apply-partially #'string-prefix-p "fred:")
                      (-drop 2 strings)))))


;;; Tests for `aph/with-advice'
;;;============================
(ert-deftest aph/advice-test-basic ()
  "Test basic functionality of `aph/with-advice'."
  (defun aph/advice-test-foo () :foo)
  (defun aph/advice-test-bar () :bar)
  (unwind-protect
      (progn (aph/with-advice
                 ((#'aph/advice-test-foo :override #'aph/advice-test-bar))
               ;; Test that function is advised
               (should (advice-member-p #'aph/advice-test-bar
                                        #'aph/advice-test-foo))
               (should (equal (aph/advice-test-foo) :bar)))
             ;; Test cleanup
             (should-not (advice-member-p #'aph/advice-test-bar
                                          #'aph/advice-test-foo))
             (should (equal (aph/advice-test-foo) :foo)))
    (unintern 'aph/advice-test-foo)
    (unintern 'aph/advice-test-bar)))

(ert-deftest aph/advice-test-lambda ()
  "Test that `aph/with-advice' works with lambdas."
  (defun aph/advice-test-foo () :foo)
  (let ((bar (lambda () :bar)))
    (unwind-protect
        (progn (aph/with-advice
                   ((#'aph/advice-test-foo :override bar))
                 ;; Test that function is advised
                 (should (advice-member-p bar #'aph/advice-test-foo))
                 (should (equal (aph/advice-test-foo) :bar)))
               ;; Test cleanup
               (should-not (advice-member-p bar #'aph/advice-test-foo))
               (should (equal (aph/advice-test-foo) :foo)))
      (unintern 'aph/advice-test-foo))))

(ert-deftest aph/advice-test-named ()
  "Test that `aph/with-advice' works with named advice."
  (defun aph/advice-test-foo () :foo)
  (let ((bar (lambda () :bar)))
    (unwind-protect
        (progn (aph/with-advice
                   ((#'aph/advice-test-foo :override bar '((name . ad-bar))))
                 ;; Test that function is advised
                 (should (advice-member-p 'ad-bar #'aph/advice-test-foo))
                 (should (equal (aph/advice-test-foo) :bar)))
               ;; Test cleanup
               (should-not (advice-member-p 'ad-bar #'aph/advice-test-foo))
               (should (equal (aph/advice-test-foo) :foo)))
      (unintern 'aph/advice-test-foo))))

(ert-deftest aph/advice-test-option-genname ()
  "Test :genname keyword in `aph/with-advice'."
  (defun aph/advice-test-foo () :foo)
  (let* ((log  nil)
         (bar  (lambda () (push :bar log))))
    (unwind-protect
        (progn (aph/with-advice
                   ((#'aph/advice-test-foo :override bar))
                 (aph/with-advice
                     ((:genname #'aph/advice-test-foo :before bar))
                   ;; Test that both pieces of advice are in effect.
                   (should (advice-member-p bar #'aph/advice-test-foo))
                   (should (equal (aph/advice-test-foo) '(:bar :bar))))
                 ;; Outer advice should still be in effect.
                 (should (advice-member-p bar #'aph/advice-test-foo))
                 (setq log nil)
                 (should (equal (aph/advice-test-foo) '(:bar)))))
      (unintern 'aph/advice-test-foo))))

      
(provide 'aph-advice-test)
