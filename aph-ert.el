;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; ERT EXTENSIONS
;;;;============================================================================

;; Functions implementing general-purpose testing apparatus for use
;; with `ert'.
(require 'symbol)
(require 'ert-x)


;;; Mode Testing Apparatus
;;;=======================
(defmacro aph/ert--with-test-mode (name maker &rest body)
  "Subroutine used to generate temporary modes for testing.

More specifically:
- Use `cl-gensym' to construct a name guaranteed not to already be in
  use, and call MAKER (a function) with that name as an argument.
  MAKER should create a mode (either major or minor) in some way.
- Execute BODY, with the name of the new mode bound to NAME (a
  symbol).  Also bind the hook variable associated with the mode to
  the symbol NAME-hook, and the keymap to NAME-map.
- Make sure the mode created does not persist outside this form, using
  `unwind-protect' to ensure it is deleted in the event of an error or
  nonlocal exit from BODY."
  (declare (debug (symbolp form body))
           (indent 2))
  (let ((hook        (symbol-concat name "-hook"))
        (keymap-var  (make-symbol "keymap-var"))
        (keymap      (symbol-concat name "-map")))
    `(let* ((,name        (cl-gensym "mode"))
            (,hook        (symbol-concat ,name "-hook"))
            (,keymap-var  (symbol-concat ,name "-map")) 
            ,keymap)
       (unwind-protect
           (progn (funcall ,maker ,name)
                  (setq ,keymap (symbol-value ,keymap-var))
                  ,@body)
         (unintern ,hook)
         (unintern ,keymap-var)
         (unintern (symbol-concat ,name "-syntax-table"))
         (unintern (symbol-concat ,name "-abbrev-table"))))))

(defmacro aph/ert-with-major-mode (name parent &rest body)
  "Execute BODY in an environment with a temporarily-defined major mode.

More specifically:
- Use `cl-gensym' to construct a name guaranteed not to already be in
  use, and create a major mode (using `define-derived-mode') with this
  name.  Evaluate PARENT to get the name of the parent mode.
- Execute BODY, with the name of the new mode bound to NAME (a
  symbol).  Also bind the hook variable associated with the mode to
  the symbol NAME-hook, and the keymap to NAME-map.
- Make sure the mode created does not persist outside this form, using
  `unwind-protect' to ensure it is deleted in the event of an error or
  nonlocal exit from BODY.

Note that the major mode constructed in this block doesn't actually do
anything (i.e., its body is empty)."
  (declare (debug (symbolp form body))
           (indent 2))
  `(aph/ert--with-test-mode ,name
       (lambda (child)
         (eval `(define-derived-mode ,child ,,parent "Lighter")))
     ,@body))

(defmacro aph/ert-with-minor-mode (name &rest body)
  "Execute BODY in an environment with a temporarily-defined minor mode.

More specifically:
- Use `cl-gensym' to construct a name guaranteed not to already be in
  use, and create a minor mode (using `define-minor-mode') with this
  name.
- Execute BODY, with the name of the new mode bound to NAME (a
  symbol).  Also bind the hook variable associated with the mode to
  the symbol NAME-hook, and the keymap to NAME-map.
- Make sure the mode created does not persist outside this form, using
  `unwind-protect' to ensure it is deleted in the event of an error or
  nonlocal exit from BODY.

Note that the minor mode constructed in this block doesn't actually do
anything (i.e., its body is empty)."
  (declare (debug (symbolp body))
           (indent 1))
  (let ((mode-name  (make-symbol "mode-name")))
    `(let (,mode-name)
       (aph/ert--with-test-mode ,name
           (lambda (mode)
             (eval `(define-minor-mode ,mode "Doc"
                      :keymap (make-sparse-keymap))))
         (unwind-protect (progn (setq ,mode-name ,name)
                                ,@body)
           (setq minor-mode-map-alist
                 (assq-delete-all ,mode-name
                                  minor-mode-map-alist)))))))


(provide 'aph-ert)
