;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; LISP CONFIGURATION
;;;;============================================================================


;;; Mode-Bundling Setup
;;;====================
;; We just need to define singleton modes here.  Languages with more
;; than one mode will do their own mode bundling and add those modes
;; to this list later.
(defvar aph/lisp-mode-hooks '(lisp-mode-hook)
  "A list of hooks for modes in which Lisp editing occurs.
Can be used with `aph/add-hook-to-all' to easily add a hook to
all such modes.")


;;; Using Specific Lisps
;;;=====================
(aph/require-softly 'init-lisp-clojure)
(aph/require-softly 'init-lisp-emacs)


;;; All-Lisp Minor Modes
;;;=====================
(aph/add-hook-to-all aph/lisp-mode-hooks #'smartparens-strict-mode :safely)
(aph/add-hook-to-all aph/lisp-mode-hooks #'rainbow-delimiters-mode :safely)

(require 'init-color-identifiers)
(aph/add-hook-to-all aph/lisp-mode-hooks #'color-identifiers-mode  :safely)

(provide 'init-lisp)
