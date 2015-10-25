;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; CUSTOM FUNCTIONS
;;;;============================================================================ 

(require 'dash)                         ; For `->>'
;; Some functions in this file also require 'cl-lib at runtime.


;;; Macros
;;;=======
(defmacro aph/defun-dyn (&rest def)
  "As `defun', but always in dynamic scope.
A function defined in this way ignores the value of
`lexical-binding' and treats it as if it were nil.

\(fn NAME ARGLIST &optional DOCSTRING DECL &rest BODY)"
  (declare (debug defun)
           (indent defun)
           (doc-string 3))
  `(eval '(defun ,@def) nil))


;;; General Advice
;;;===============
(defun aph/preserving-text-scale (fn &rest args)
  "Call FN with ARGS, preserving current text scaling."
  (require 'face-remap)                 ; For `text-scale-mode-amount'
  (let ((scale text-scale-mode-amount))
    (apply fn args)
    (text-scale-set scale)))


;;; Utility Functions
;;;==================
(defun aph/assoc-delete-all (key alist)
  "As `assq-delete-all', but use `equal' rather than `eq'."
  (require 'cl-lib)                       ; For `cl-delete'
  (cl-delete key alist :test #'equal :key #'car))

(defun aph/canary (&rest args)
  "Print a message containing ARGS."
  (message "Canary called with args %s" args))

;; Taken from the Yoo Box article
;; "Emacs Lisp lexical binding gotchas and related best practices" 
(defmacro aph/lexical-scope-p (var)
  "Returns t if VAR can be lexically bound, and nil otherwise.

Specially, this will return nil when called in dynamic scope, and
it will return nil if var has been declared as a special
variable (e.g., with `defvar').  All other cases should return
t."
  `(let ((,var nil)
         (f (let ((,var t)) (lambda () ,var))))
     (funcall f)))

(defun aph/reduce (fn seq)
  "As `-reduce', but accept all sequences, not just lists."
  (require 'dash)                       ; For `-reduce'
  (if (listp seq)
      (-reduce fn seq)
    (-reduce fn (append seq nil))))

(defun aph/reductions (fn seq)
  "As `aph/reduce', but return intermediate results.
These results are returned as a list."
  (->> seq
       (aph/reduce (lambda (acc val) 
                     (let ((acc (if (listp acc) acc (list acc))))
                       (cons (funcall fn (car acc) val) acc))))
       (reverse)))

(provide 'aph-lib)
