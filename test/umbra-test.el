;;; umbra-test.el --- Tests for umbra.el             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `umbra', `dash', `symbol', `aph-ert-test'

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

(require 'umbra)
(require 'symbol)
(require 'aph-ert-test)


;;; Testing Apparatus
;;;==================
(defmacro umbra-with-mode (name parent penumbra &rest body)
  "Execute BODY with temporarily-defined umbra mode.

As `aph/ert-with-major-mode', but in addition an umbra keymap is
created with `umbra-keymap', and inside BODY the variable
NAME-umbra-map is bound to the mode's umbra keymap.

As a special case, if PARENT is the keyword :minor, then the mode
instantiated is a minor mode; see `aph/ert-with-minor-mode'.

If PENUMBRA is non-nil, the penumbra keymap used instead (see
`umbra-keymap' for more details on this); otherwise, it is the
ordinary, non-overriding keymap. The same name (NAME-umbra-map)
is used, even for the penumbra keymap.

Note that, even though only one keymap (either the umbra or the
penumbra) is bound, both keymap variables are uninterned during
cleanup.  This means that you can safely create the other keymap
inside BODY, without any additional cleanup required."
  (declare (debug aph/ert-with-major-mode)
           (indent 3))
  (let ((macro-form     (if (eq parent :minor)
                            `(aph/ert-with-minor-mode ,name)
                          `(aph/ert-with-major-mode ,name ,parent)))
        (umbra-map      (symbol-concat name "-umbra-map"))
        (umbra-map-var  (make-symbol "umbra-map-var"))
        (other-map-var  (make-symbol "other-map-var"))
        (mode-name      (make-symbol "mode-name")))
    `(let (,mode-name)
       (,@macro-form
        (let* ((,umbra-map-var  (umbra-keymap-var  ,name ,penumbra))
               (,other-map-var  (umbra-keymap-name ,name ,(not penumbra)))
               (,umbra-map      (umbra-keymap      ,name ,penumbra)))
          (unwind-protect (progn (setq ,mode-name ,name)
                                 ,@body)
            (unintern ,umbra-map-var)
            (unintern ,other-map-var)
            (setq umbra-map-alist
                  (assq-delete-all ,mode-name umbra-map-alist))))))))


;;; Apparatus Tests
;;;================
(defmacro umbra-test-with-mode--all-params (&rest body)
  "Macro for testing `umbra-with-mode'.

Execute BODY in an environment where the variables 'parent' and
'override' are bound to all four combinations such that 'parent'
is either the quoted symbol 'fundamental-mode or the keyword :minor
and 'penumbra' is either the keyword :penumbra or nil."
  (declare (indent 0) (debug body))
  `(dolist (parent '('fundamental-mode :minor))
     (dolist (penumbra'(:penumbra nil))
       ,@body)))

(ert-deftest umbra-test-with-mode--body ()
  "Test that `umbra-with-mode' evaluates its body."
  (umbra-test-with-mode--all-params
   (should (aph/ert-macro-executes-body
            'umbra-with-mode
            `(mode ,parent ,penumbra)))))

(ert-deftest umbra-test-with-mode--bindings ()
  "Test bindings of `umbra-with-mode'."
  (umbra-test-with-mode--all-params
    (should (aph/ert-test-mode-wrapper--bindings
             'umbra-with-mode
             `(,parent ,penumbra)))
    (umbra-with-mode mode parent penumbra
      (should (umbra-has-keymap-p mode penumbra)))))

(ert-deftest umbra-test-with-mode--cleanup ()
  "Test cleanup for `umbra-with-mode'."
  (umbra-test-with-mode--all-params
    (should (aph/ert-test-mode-wrapper--cleanup
             'umbra-with-mode
             `(,parent ,penumbra)))
    (should (aph/ert-macro-does-not-leak
             'umbra-with-mode
             ''mode-umbra-map
             `(mode ,parent ,penumbra))) 
    (let (mode-x)
      (umbra-with-mode mode parent penumbra
        (setq mode-x mode)
        ;; Penumbra maps are not registered in `umbra-map-alist', so
        ;; this next test says that a mode is registered there iff it
        ;; is not an penumbra map.
        (should (eq (not (null penumbra))
                    (null (assoc mode-x umbra-map-alist)))))
      (should-not (assoc mode-x umbra-map-alist))
      ;; Finally, verify that both keymaps are removed.
      (should-not (umbra-has-keymap-p mode-x nil))
      (should-not (umbra-has-keymap-p mode-x :penumbra)))))


;;; Tests for `emulation-mode-map-alists' setup
;;;============================================
(ert-deftest umbra-test-emma-setup ()
  "Test `emulation-mode-map-alists' setup for `umbra-mode'."
  (require 'dash)      ; For `-is-infix-p'
  ;; The symbols `umbra-overriding-map-alist',
  ;; `umbra-minor-mode-map-alist', and `umbra-local-map-alist' should
  ;; appear in `emulation-mode-map-alists', and they should appear in
  ;; that order.  For the sake of simplicity, we also assume that they
  ;; appear consecutively.
  (should (-is-infix-p '(umbra-overriding-map-alist
                         umbra-minor-mode-map-alist
                         umbra-local-map-alist)
                       emulation-mode-map-alists)))


;;; Keymap Tests
;;;=============
(ert-deftest umbra-test-keymap-creation ()
  "Test `umbra-keymap' and `umbra-keymap-var'."
  (umbra-test-with-mode--all-params
    (unless (and penumbra (eq parent :minor))
      (umbra-with-mode mode parent penumbra
        (should (eq mode-umbra-map
                    (umbra-keymap mode penumbra)))
        (should (eq mode-umbra-map
                    (symbol-value (umbra-keymap-var mode penumbra))))
        (should (equal (umbra-keymap-var mode penumbra)
                       (umbra-keymap-name mode penumbra)))
        (define-key mode-umbra-map (kbd "a") #'ignore)
        (should (eq (lookup-key (umbra-keymap mode penumbra) (kbd "a"))
                    #'ignore))))))

(ert-deftest umbra-test-has-keymap-p ()
  "Test `umbra-has-keymap-p'."
  (dolist (penumbra '(:penumbra nil))
    (let* ((mode  'foo-mode)
           var)
      (should-not (umbra-has-keymap-p mode penumbra))
      (setq var (umbra-keymap-var mode penumbra))
      (unwind-protect
          (should (umbra-has-keymap-p mode penumbra))
        (unintern var)
        (setq umbra-map-alist (assq-delete-all mode umbra-map-alist))))))


;;; Keybinding Precedence Tests
;;;============================
(ert-deftest umbra-test-mode-bindings ()
  "Test mode bindings in `umbra-mode'."
  (umbra-with-mode test-major-mode 'fundamental-mode nil
    (umbra-with-mode test-minor-mode :minor nil
      (let ((umbra-mode nil))
        (with-temp-buffer
          ;; Set up keybindings
          (define-key test-major-mode-map (kbd "a") #'foo-major)
          (define-key test-minor-mode-map (kbd "a") #'foo-minor)
          (define-key test-major-mode-umbra-map (kbd "a") #'umbra-major)
          (define-key test-minor-mode-umbra-map (kbd "a") #'umbra-minor)
          ;; Test keybindings (in Gray code order) 
          (should (eq (key-binding (kbd "a")) #'self-insert-command))
          (umbra-mode 1)                ; 000 -> 001
          (funcall test-minor-mode 1)   ; 001 -> 011
          (should (eq (key-binding (kbd "a")) #'umbra-minor))
          (umbra-mode -1)               ; 011 -> 010
          (should (eq (key-binding (kbd "a")) #'foo-minor))
          (funcall test-major-mode)     ; 010 -> 110
          (funcall test-minor-mode 1)
          (should (eq (key-binding (kbd "a")) #'foo-minor))
          (umbra-mode 1)                ; 110 -> 111
          (should (eq (key-binding (kbd "a")) #'umbra-minor))
          (funcall test-minor-mode -1)  ; 111 -> 101
          (should (eq (key-binding (kbd "a")) #'umbra-major))
          (umbra-mode -1)               ; 101 -> 100
          (should (eq (key-binding (kbd "a")) #'foo-major)))))))

(ert-deftest umbra-test-penumbra ()
  "Test penumbra bindings in `umbra-mode'."
  (umbra-with-mode test-major-mode 'fundamental-mode nil
    (umbra-with-mode test-minor-mode :minor nil
      (let ((test-major-mode-penumbra-map
             (umbra-keymap test-major-mode :penumbra))
            (umbra-mode nil))
        (with-temp-buffer
          ;; Define keybindings
          (define-key test-major-mode-umbra-map (kbd "a") #'major)
          (define-key test-minor-mode-umbra-map (kbd "a") #'minor)
          (define-key test-major-mode-penumbra-map  (kbd "a") #'penumbra)
          ;; Test keybindings
          (should (eq (key-binding (kbd "a")) #'self-insert-command))
          (umbra-mode 1)
          (funcall test-minor-mode 1)
          (should (eq (key-binding (kbd "a")) #'minor))
          (funcall test-major-mode)
          (should (eq (key-binding (kbd "a")) #'penumbra)))))))

(ert-deftest umbra-test-mode-parentage ()
  "Test that mode parentage is correct in `umbra-mode'."
  (umbra-with-mode mode1 'fundamental-mode nil
    (let ((umbra-mode t)
          mode2-name)
      (aph/ert-with-major-mode mode2 mode1
        (setq mode2-name mode2)
        (unwind-protect
            (with-temp-buffer
              (define-key mode1-umbra-map (kbd "a") #'foo)
              (funcall mode2)
              (should (eq (key-binding (kbd "a")) #'foo)))
          (dolist (override '(t nil))
            (unintern (umbra-keymap-name mode2-name override)))
          (setq umbra-map-alist
                (assq-delete-all mode2-name umbra-map-alist)))))))


;;; `bind-keys' Support Machinery Tests
;;;====================================
(ert-deftest umbra-test-bind-keys--basic ()
  "Basic test for `bind-keys' keywords :umbra and :penumbra."
  (let ((test (lambda (mode penumbra)
                (eval `(bind-keys ,(if penumbra :penumbra :umbra) ,mode
                                  ("a" . foo)))
                (should (eq (lookup-key (umbra-keymap mode penumbra)
                                        (kbd "a"))
                            #'foo))
                (should (assoc `("a" . ,(umbra-keymap-name mode penumbra))
                               personal-keybindings))))
        (personal-keybindings nil))
    (dolist (penumbra '(t nil))
      ;; With mode already augmented
      (dolist (param '('fundamental-mode :minor)) 
        (umbra-with-mode foo-mode param penumbra
          (funcall test foo-mode penumbra)))
      ;; With mode not previously augmented
      (aph/ert-with-minor-mode foo-mode
        (setq var (umbra-keymap-name foo-mode penumbra))
        (unwind-protect (funcall test foo-mode penumbra)
          (unintern var)
          (setq umbra-map-alist
                (assq-delete-all foo-mode umbra-map-alist)))))))

(ert-deftest umbra-test-bind-keys--multiple ()
  "Test `bind-keys' keywords :umbra and :penumbra with multiple maps."
    (umbra-with-mode foo-mode :minor nil
      (umbra-with-mode bar-mode :minor :penumbra
        (aph/ert-with-minor-mode baz-mode
          (aph/ert-with-minor-mode quux-mode
            (let ((personal-keybindings nil)
                  (baz-mode-map-var    (symbol-concat baz-mode "-map"))
                  (quux-mode-map-var   (symbol-concat quux-mode "-map")))
              (eval `(bind-keys :umbra ,foo-mode
                                :penumbra ,bar-mode
                                :map (,baz-mode-map-var ,quux-mode-map-var)
                                ("a" . foo)))
              (dolist (keymap (list foo-mode-umbra-map
                                    bar-mode-umbra-map
                                    baz-mode-map
                                    quux-mode-map))
                (should (eq (lookup-key keymap (kbd "a"))
                            'foo)))))))))


(provide 'umbra-test)
;;; umbra-test.el ends here
