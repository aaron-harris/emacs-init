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
