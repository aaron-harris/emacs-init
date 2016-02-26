;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; APH-KEYS TESTS
;;;;============================================================================

;; Tests for the module `aph-keys'.
(require 'aph-keys)
(require 'aph-ert)
(require 'aph-ert-test)


;;; Testing Apparatus
;;;==================
(defmacro aph-keys-with-augmented-mode (name parent override &rest body)
  "Execute BODY with temporarily-defined augmented mode.

As `aph/ert-with-major-mode', but in addition mode is augmented
using `aph/keys-augment', and inside body the variable
NAME-augmented-map is bound to the mode's augmented keymap.

As a special case, if PARENT is the keyword :minor, then the mode
instantiated is a minor mode; see `aph/ert-with-minor-mode'.

If OVERRIDE is non-nil, the augmented keymap used is the
overriding one (see `aph/keys-augment' for more details on this);
otherwise, it is the ordinary, non-overriding keymap.

Note that, even though only one keymap (either overriding or not)
is bound, both keymap variables are uninterned during cleanup.
This means that you can safely augment the mode again inside BODY
to get the second keymap, without any additional cleanup."
  (declare (debug aph/ert-with-major-mode)
           (indent 3))
  (let ((macro-form  (if (eq parent :minor)
                         `(aph/ert-with-minor-mode ,name)
                       `(aph/ert-with-major-mode ,name ,parent)))
        (augmap       (aph/symbol-concat name "-augmented-map"))
        (augmap-var   (make-symbol "augmap-var"))
        (augmap-other (make-symbol "augmap-other"))
        (mode-name    (make-symbol "mode-name")))
    `(let (,mode-name)
       (,@macro-form
        (let* ((,augmap-var   (aph-keys-augment-var  ,name ,override))
               (,augmap-other (aph-keys-augment-name ,name ,(not override)))
               (,augmap       (aph-keys-augment ,name ,override)))
          (unwind-protect (progn (setq ,mode-name ,name)
                                 ,@body)
            (unintern ,augmap-var)
            (unintern ,augmap-other)
            (setq aph-keys-augment-map-alist
                  (assq-delete-all ,mode-name
                                   aph-keys-augment-map-alist))))))))


;;; Apparatus Tests
;;;================
(defmacro aph-keys-test-with-augmented-mode--all-params (&rest body)
  "Macro for testing `aph-keys-with-augmented-mode'.

Execute BODY in an environment where the variables 'parent' and
'override' are bound to all four combinations such that 'parent'
is either the quoted symbol 'fundamental-mode or the
keyword :minor and 'override' is either the keyword :override or
nil."
  (declare (indent 0) (debug body))
  `(dolist (parent '('fundamental-mode :minor))
     (dolist (override '(:override nil))
       ,@body)))

(ert-deftest aph-keys-test-with-augmented-mode--body ()
  "Test that `aph-keys-with-augmented-mode' evaluates its body."
  (aph-keys-test-with-augmented-mode--all-params
    (should (aph/ert-macro-executes-body
             'aph-keys-with-augmented-mode
             `(mode ,parent ,override)))))

(ert-deftest aph-keys-test-with-augmented-mode--bindings ()
  "Test bindings of `aph-keys-with-augmented-mode'."
  (aph-keys-test-with-augmented-mode--all-params
    (should (aph/ert-test-mode-wrapper--bindings
             'aph-keys-with-augmented-mode
             `(,parent ,override)))
    (aph-keys-with-augmented-mode mode parent override
      (should (aph-keys-augmented-p mode override)))))

(ert-deftest aph-keys-test-with-augmented-mode--cleanup ()
  "Test cleanup for `aph-keys-with-augmented-mode'."
  (aph-keys-test-with-augmented-mode--all-params
    (should (aph/ert-test-mode-wrapper--cleanup
             'aph-keys-with-augmented-mode
             `(,parent ,override)))
    (should (aph/ert-macro-does-not-leak
             'aph-keys-with-augmented-mode
             ''mode-augmented-map
             `(mode ,parent ,override))) 
    (let (mode-x)
      (aph-keys-with-augmented-mode mode parent override
        (setq mode-x mode)
        ;; Override maps are not registered in
        ;; `aph-keys-augment-map-alist', so this next test says that a
        ;; mode is registered there iff it is not an override map.
        (should (eq (not (null override))
                    (null (assoc mode-x aph-keys-augment-map-alist))))) 
      (should-not (assoc mode-x aph-keys-augment-map-alist))
      ;; Finally, verify that both augmented keymaps are removed.
      (should-not (aph-keys-augmented-p mode-x nil))
      (should-not (aph-keys-augmented-p mode-x :override)))))


;;; Tests for `emulation-mode-map-alists' setup
;;;============================================
(ert-deftest aph-keys-test-emma-setup ()
  "Test `emulation-mode-map-alists' setup for `aph-keys-mode'."
  (require 'dash)      ; For `-is-infix-p'
  ;; The symbols `aph-keys-overriding-map-alist',
  ;; `aph-keys-minor-mode-map-alist', and `aph-keys-local-map-alist'
  ;; should appear in `emulation-mode-map-alists', and they should
  ;; appear in that order.  For the sake of simplicity, we also assume
  ;; that they appear consecutively.
  (should (-is-infix-p '(aph-keys-overriding-map-alist
                         aph-keys-minor-mode-map-alist
                         aph-keys-local-map-alist)
                       emulation-mode-map-alists)))


;;; Augmented Keymap Tests
;;;================================
(ert-deftest aph-keys-test-augment ()
  "Test `aph-keys-augment' and `aph-keys-augment-var'."
  (require 'aph-dash)                   ; For `aph/eq'
  (aph-keys-test-with-augmented-mode--all-params
    (unless (and override (eq parent :minor))
      (aph-keys-with-augmented-mode mode parent override
        (should (aph/eq mode-augmented-map
                        (aph-keys-augment mode override)
                        (symbol-value (aph-keys-augment-var mode override))))
        (should (equal (aph-keys-augment-var mode override)
                       (aph-keys-augment-name mode override)))
        (define-key mode-augmented-map (kbd "a") #'ignore)
        (should (eq (lookup-key (aph-keys-augment mode override) (kbd "a"))
                    #'ignore))))))

(ert-deftest aph-keys-test-augmented-p ()
  "Test `aph-keys-augmented-p'."
  (dolist (override '(:override nil))
    (let* ((mode  'foo-mode)
           var)
      (should-not (aph-keys-augmented-p mode override))
      (setq var (aph-keys-augment-var mode override))
      (unwind-protect
          (should (aph-keys-augmented-p mode override))
        (unintern var)
        (setq aph-keys-augment-map-alist
              (assq-delete-all mode aph-keys-augment-map-alist))))))


;;; Keybinding Precedence Tests
;;;============================
(ert-deftest aph-keys-test-mode-bindings ()
  "Test mode bindings in `aph-keys-mode'."
  (aph-keys-with-augmented-mode test-major-mode 'fundamental-mode nil
    (aph-keys-with-augmented-mode test-minor-mode :minor nil
      (let ((aph-keys-mode nil))
        (with-temp-buffer
          ;; Set up keybindings
          (define-key test-major-mode-map (kbd "a") #'foo-major)
          (define-key test-minor-mode-map (kbd "a") #'foo-minor)
          (define-key test-major-mode-augmented-map (kbd "a") #'aug-major)
          (define-key test-minor-mode-augmented-map (kbd "a") #'aug-minor)
          ;; Test keybindings (in Gray code order) 
          (should (eq (key-binding (kbd "a")) #'self-insert-command))
          (aph-keys-mode 1)             ; 000 -> 001
          (funcall test-minor-mode 1)   ; 001 -> 011
          (should (eq (key-binding (kbd "a")) #'aug-minor))
          (aph-keys-mode -1)            ; 011 -> 010
          (should (eq (key-binding (kbd "a")) #'foo-minor))
          (funcall test-major-mode)     ; 010 -> 110
          (funcall test-minor-mode 1)
          (should (eq (key-binding (kbd "a")) #'foo-minor))
          (aph-keys-mode 1)             ; 110 -> 111
          (should (eq (key-binding (kbd "a")) #'aug-minor))
          (funcall test-minor-mode -1)  ; 111 -> 101
          (should (eq (key-binding (kbd "a")) #'aug-major))
          (aph-keys-mode -1)            ; 101 -> 100
          (should (eq (key-binding (kbd "a")) #'foo-major)))))))

(ert-deftest aph-keys-test-override ()
  "Test overriding bindings in `aph-keys-mode'."
  (aph-keys-with-augmented-mode test-major-mode 'fundamental-mode nil
    (aph-keys-with-augmented-mode test-minor-mode :minor nil
      (let ((test-major-mode-override-map
             (aph-keys-augment test-major-mode :override))
            (aph-keys-mode nil))
        (with-temp-buffer
          ;; Define keybindings
          (define-key test-major-mode-augmented-map (kbd "a") #'major)
          (define-key test-minor-mode-augmented-map (kbd "a") #'minor)
          (define-key test-major-mode-override-map  (kbd "a") #'override)
          ;; Test keybindings
          (should (eq (key-binding (kbd "a")) #'self-insert-command))
          (aph-keys-mode 1)
          (funcall test-minor-mode 1)
          (should (eq (key-binding (kbd "a")) #'minor))
          (funcall test-major-mode)
          (should (eq (key-binding (kbd "a")) #'override)))))))

(ert-deftest aph-keys-test-mode-parentage ()
  "Test that mode parentage is correct in `aph-keys-mode'."
  (aph-keys-with-augmented-mode mode1 'fundamental-mode nil
    (let (mode2-name)
      (aph/ert-with-major-mode mode2 mode1
        (setq mode2-name mode2)
        (unwind-protect
            (with-temp-buffer
              (define-key mode1-augmented-map (kbd "a") #'foo)
              (funcall mode2)
              (should (eq (key-binding (kbd "a")) #'foo)))
          (dolist (override '(t nil))
            (unintern (aph-keys-augment-name mode2-name override)))
          (setq aph-keys-augment-map-alist
                (assq-delete-all mode2-name
                                 aph-keys-augment-map-alist)))))))


;;; `bind-keys' Support Machinery Tests
;;;====================================
(ert-deftest aph-keys-test-bind-keys ()
  "Test functionality of `aph/bind-keys'"
  (let ((test (lambda (mode)
                (eval `(bind-keys :augment ,mode
                                  ("a" . foo))) 
                (should (eq (lookup-key (aph-keys-augment mode) (kbd "a"))
                            #'foo))
                (should (assoc `("a" . ,(aph-keys-augment-name mode))
                               personal-keybindings))))
        (personal-keybindings nil))
    ;; With mode already augmented
    (dolist (param '('fundamental-mode :minor)) 
      (aph-keys-with-augmented-mode foo-mode param
        (funcall test foo-mode)))
    ;; With mode not previously augmented
    (aph/ert-with-minor-mode foo-mode
      (setq var (aph-keys-augment-name foo-mode))
      (unwind-protect (funcall test foo-mode) 
        (unintern var)
        (setq aph-keys-augment-map-alist
              (assq-delete-all foo-mode aph-keys-augment-map-alist))))))        
        

(provide 'aph-keys-test)
