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


;;; Improvements to C-h F
;;;======================
(defun aph/Info-find-manual-to-search (symbol &optional is-var)
  "Find the Info manual most likely to describe SYMBOL.

Here SYMBOL is the name of a function or variable.  If SYMBOL is
both the name of a function and the name of a variable, it is
interpreted as the function name unless the optional parameter
IS-VAR is non-nil."
  (let ((res (Info-find-emacs-command-nodes symbol)))
    (cond
     (res     (car res))
     (is-var  "emacs")
     (t       "elisp"))))

(defun aph/Info-goto-emacs-function-node (function)
  "Go to the Info node in the Emacs manual for FUNCTION.

Commands are passed directly to `Info-goto-emacs-command-node'.
For other functions, an index search is attempted."
  (interactive "aFind documentation for function: ")
  (let ((res nil))
    (cond
     ;; Send commands to `Info-goto-emacs-command-node'.
     ((commandp function)
      (Info-goto-emacs-command-node function))
     ;; Then see if `Info-find-emacs-command-nodes' finds anything.
     ;; This function is only documented to work for commands, but
     ;; seems to work fine for all functions and variables.
     ((Info-find-emacs-command-nodes function))
     ;; Finally, try an index search in an appropriate manual.
     (t
      (info (aph/Info-find-manual-to-search function))
      (Info-index (symbol-name function))))))

;; This will mostly follow `aph/Info-goto-emacs-function-node', once
;; that's working.
(defun aph/Info-goto-emacs-variable-node (variable)
  "Go to the Info node in the Emacs manual for VARIABLE."
  (interactive "vFind documentation for variable: ")
  (info (aph/Info-find-manual-to-search variable :var))
  (Info-index (symbol-name variable)))


;;; Adding font lock for `aph/defun-dyn'
;;;=====================================
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\(\\(aph/defun-dyn\\_>\\)" 1 font-lock-keyword-face)))


;;; Freeing `C-[' and `C-i'
;;;========================
(define-key input-decode-map [?\C-\[] (kbd "<C-[>"))


;;; Suspend updates for `helm-projectile-grep'
;;;===========================================
(defun aph/helm-suspend-update-initially ()
  "Suspend helm updates, and remove self from `helm-update-hook'.

This function can be added to `helm-update-hook' immediately
before a helm command is called.  Doing so will suspend updates
for that command."
  (helm-toggle-suspend-update)
  (remove-hook 'helm-update-hook #'aph/helm-suspend-update-initially))

(defun aph/helm-with-suspended-updates (fun &rest args)
  "Call FUN with ARGS, suspending helm updates."
  (unwind-protect
      (progn
        (add-hook 'helm-update-hook #'aph/helm-suspend-update-initially)
        (apply fun args))
    (remove-hook 'helm-update-hook #'aph/helm-suspend-update-initially)))

(ert-deftest aph/helm-test-suspend-updates ()
  "Test `aph/helm-with-suspended-updates' and related functions."
  (let* ((source    (helm-build-async-source "Foo"))
         (helm-call (lambda (&rest args)
                      (helm :sources source
                            :candidates args)))
         (helm-update-hook nil))
    (aph/helm-with-suspended-updates helm-call 'foo)
    (should (null helm-update-hook))))

(defun aph/helm-projectile-grep (&optional dir)
  "As `helm-projectile-grep', with updates suspended."
  (interactive)
  (aph/helm-with-suspended-updates #'helm-projectile-grep dir))

(defun aph/foo ()
  "Test constructing helm commands."
  (interactive)
  (helm :sources (helm-build-async-source "Foo")))

(remove-hook 'helm-update-hook #'aph/canary)


;;; Message Suppression
;;;====================
(defvar aph/silence-list nil
  "A list identifying messages to be silenced.

Valid entries are strings and functions.

A string entry is interpreted as a regexp; messages containing a match
will be silenced.  Note that the entire message doesn't have to
match.

A function entry is called for each message, with the same args
as the triggering `message' call, and the message is silenced if
the function returns non-nil.

See `aph/silence-advice' for more details.")

(defvar aph/silence-enabled nil
  "If non-nil, some messages will be silenced.

To control which messages are silenced, see `aph/silence-list'.
See `aph/silence-advice' for more details.")

(defun aph/silence-message-p (msg)
  "Return non-nil if MSG should be silenced per `aph/silence-list'.
If `aph/silence-enabled' is nil, return nil for any MSG.  See
`aph/silence-advice' for more details.

MSG should be a string, already formatted, just as it would
appear in the *Messages* buffer."
  (require 'dash)                       ; For `-some-p'
  (when aph/silence-enabled
    (-some-p
     (lambda (elt)
       (cond
        ((stringp elt)    (string-match-p elt msg))
        ((functionp elt)  (funcall elt msg))
        :else             (error "Invalid entry %s in `aph/silence-list'" elt)))
     aph/silence-list)))

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

(defun aph/silence-advice (fun &rest args)
  "Advice to enforce `aph/silence-enabled'.

If `aph/silence-enabled' is nil, do nothing.

Otherwise, consult `aph/silence-list' to see whether the message
defined by ARGS should be silenced (where ARGS is the arglist for
the pending `message' command.

If a message is silenced, `format' is called instead of
`message'.  Thus, the return value is unchanged, but no message
is shown in the *Messages* buffer.

This function is intended as :around advice for `message'.  As
`message' is a primitive, this is not ideal, but there doesn't
seem to be any way to achieve this kind of message suppression
otherwise.  Because `message' is a primitive, not all messages
can be silenced; calls from C code may avoid being silenced." 
  (let ((msg (apply #'format args)))
    (if (aph/silence-message-p msg)
        msg
      (apply fun args))))

(advice-add #'message :around #'aph/silence-advice)

;; At present, this does not leave `aph/silence-advice' as advice on
;; `message' if it was already there!  We need to tweak
;; `aph/with-advice' to be more robust.
(ert-deftest aph/silence-test-advice ()
  "Test the function `aph/silence-advice'." 
  (aph/with-advice ((#'message :around #'aph/silence-advice))
    (let ((aph/silence-list    '("foo"))
          (aph/silence-enabled t))
      (should (equal (message "bar") (current-message)))
      (should-not (equal (message "foo") (current-message)))
      (should (equal (message "%s" "foo") "foo"))
      (let ((aph/silence-enabled nil))
        (should (equal (message "foo") (current-message))))
      (let ((aph/silence-list nil))
        (should (equal (message "foo") (current-message)))))))

(provide 'init-draft)
