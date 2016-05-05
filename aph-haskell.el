;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; HASKELL EXTENSIONS
;;;;============================================================================

;; Functions extending Haskell mode and related packages.


;;; Indentation
;;;============
;; Functions in this section deal with indentation modes within
;; Haskell mode.
(defun aph/haskell-indentation-mode:off ()
  "Turn off `haskell-indentation-mode'.
This is equivalent to calling `haskell-indentation-mode' with an
argument of -1 and is intended for use in hooks (where using a
`lambda' can be problematic)."
  (haskell-indentation-mode -1))


;;; Compatability
;;;==============
;; Functions in this section implement compatability for my setup.
(defun aph/haskell-process-load-file-cygwin ()
  "As `haskell-process-load-file', with Cygwin compatibility.

This function detects a Cygwin system using the variable
`system-type'; non-Cygwin systems should be unaffected." 
  (interactive)
  (require 'aph-advice)                 ; For `aph/with-advice'
  (if (and (eq system-type 'cygwin) (require 'cygwinize))
      (aph/with-advice
          ((#'buffer-file-name
            :filter-return #'cygwinize-convert-file-name-to-hybrid-windows))
        (haskell-process-load-file))
    (haskell-process-load-file)))


(provide 'aph-haskell)
