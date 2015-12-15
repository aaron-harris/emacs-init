;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; LEXICAL EXTENSIONS
;;;;============================================================================ 

;; Functions dealing directly with issues of dynamic vs. lexical
;; binding.


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


;;; Utility Functions
;;;==================
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


(provide 'aph-lexical)