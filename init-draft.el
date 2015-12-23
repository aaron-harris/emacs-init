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
