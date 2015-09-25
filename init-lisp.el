;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; LISP CONFIGURATION
;;;;============================================================================

(require 'aph-require)                  ; For `aph/require-softly'
(require 'aph-hooks)                    ; For `aph/add-hook-to-all'


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

(when (package-installed-p 'company)
  (aph/add-hook-to-all aph/lisp-mode-hooks #'company-mode :safely))


;;; Completion
;;;===========
;; The `hippie-expand' methods `try-expand-list' and `try-expand-line'
;; are usually not what I want when editing a Lisp, so let's try those
;; last.
(defun aph/hippie-expand-config-lisp ()
  "Configure `hippie-expand-try-functions-list' for Lisps."
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-list
          try-expand-line
          try-expand-line-all-buffers)))
(aph/add-hook-to-all aph/lisp-mode-hooks #'aph/hippie-expand-config-lisp)

(provide 'init-lisp)
