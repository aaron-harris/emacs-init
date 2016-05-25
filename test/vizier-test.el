;;; vizier-test.el --- Tests for vizier.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `vizier', `ert', `dash'

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'vizier)


;;; Tests for `vizier--genname'
;;;============================
(ert-deftest vizier-test-genname ()
  "Test functionality of `vizier--genname'."
  (require 'dash)
  (let* ((results  (mapcar #'vizier--genname
                           '(nil
                             ((foo . bar))
                             ((name . fred))
                             ((foo . bar) (name . fred)))))
         (names    (mapcar (apply-partially #'assoc-default 'name) results))
         (strings  (mapcar #'symbol-name names)))
    (should (-every-p #'identity names))
    (should (-every-p #'symbolp names))
    (should (-none-p #'intern-soft names))
    (should (-every-p (apply-partially #'string-prefix-p "vizier:")
                      (-take 2 strings)))
    (should (-every-p (apply-partially #'string-prefix-p "fred:")
                      (-drop 2 strings)))))


;;; Tests for `vizier-with-advice'
;;;============================
(ert-deftest vizier-test-basic ()
  "Test basic functionality of `vizier-with-advice'."
  (defun vizier-test-foo () :foo)
  (defun vizier-test-bar () :bar)
  (unwind-protect
      (progn (vizier-with-advice
                 ((vizier-test-foo :override #'vizier-test-bar))
               ;; Test that function is advised
               (should (advice-member-p #'vizier-test-bar
                                        #'vizier-test-foo))
               (should (equal (vizier-test-foo) :bar)))
             ;; Test cleanup
             (should-not (advice-member-p #'vizier-test-bar
                                          #'vizier-test-foo))
             (should (equal (vizier-test-foo) :foo)))
    (unintern 'vizier-test-foo)
    (unintern 'vizier-test-bar)))

(ert-deftest vizier-test-lambda ()
  "Test that `vizier-with-advice' works with lambdas."
  (defun vizier-test-foo () :foo)
  (let ((bar (lambda () :bar)))
    (unwind-protect
        (progn (vizier-with-advice
                   ((vizier-test-foo :override bar))
                 ;; Test that function is advised
                 (should (advice-member-p bar #'vizier-test-foo))
                 (should (equal (vizier-test-foo) :bar)))
               ;; Test cleanup
               (should-not (advice-member-p bar #'vizier-test-foo))
               (should (equal (vizier-test-foo) :foo)))
      (unintern 'vizier-test-foo))))

(ert-deftest vizier-test-named ()
  "Test that `vizier-with-advice' works with named advice."
  (defun vizier-test-foo () :foo)
  (let ((bar (lambda () :bar)))
    (unwind-protect
        (progn (vizier-with-advice
                   ((vizier-test-foo :override bar ((name . ad-bar))))
                 ;; Test that function is advised
                 (should (advice-member-p 'ad-bar 'vizier-test-foo))
                 (should (equal (vizier-test-foo) :bar)))
               ;; Test cleanup
               (should-not (advice-member-p 'ad-bar 'vizier-test-foo))
               (should (equal (vizier-test-foo) :foo)))
      (unintern 'vizier-test-foo))))

(ert-deftest vizier-test-option-once ()
  "Test :once keyword in `vizier-with-advice'."
  (defun vizier-test-foo () :foo)
  (let ((bar (lambda () :bar)))
    (unwind-protect
        (progn (vizier-with-advice
                   ((:once vizier-test-foo :override bar))
                 (should (advice-member-p bar #'vizier-test-foo))
                 (should (equal (vizier-test-foo) :bar))
                 (should (equal (vizier-test-foo) :foo)))
               (should-not (advice-member-p bar #'vizier-test-foo))
               (should (equal (vizier-test-foo) :foo)))
      (unintern 'vizier-test-foo))))

(ert-deftest vizier-test-option-genname ()
  "Test :genname keyword in `vizier-with-advice'."
  (defun vizier-test-foo () :foo)
  (let* ((log  nil)
         (bar  (lambda () (push :bar log))))
    (unwind-protect
        (progn (vizier-with-advice
                   ((vizier-test-foo :override bar))
                 (vizier-with-advice
                     ((:genname vizier-test-foo :before bar))
                   ;; Test that both pieces of advice are in effect.
                   (should (advice-member-p bar #'vizier-test-foo))
                   (should (equal (vizier-test-foo) '(:bar :bar))))
                 ;; Outer advice should still be in effect.
                 (should (advice-member-p bar #'vizier-test-foo))
                 (setq log nil)
                 (should (equal (vizier-test-foo) '(:bar)))))
      (unintern 'vizier-test-foo))))

(provide 'vizier-test)
;;; vizier-test.el ends here
