;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; HASKELL EXTENSIONS
;;;;============================================================================

;; Functions extending Haskell mode and related packages.


;;; Compatability
;;;==============
;; Functions in this section implement compatability for my setup.
(defun aph/haskell-process-load-file-cygwin ()
  "As `haskell-process-load-file', with Cygwin compatibility.

This function detects a Cygwin system using the variable
`system-type'; non-Cygwin systems should be unaffected." 
  (interactive)
  (require 'aph-advice)                 ; For `aph/with-advice'
  (if (and (eq system-type 'cygwin) (require 'aph-cygwin))
      (aph/with-advice
          ((#'buffer-file-name
            :filter-return #'aph/cygwin-convert-file-name-to-hybrid-windows))
        (haskell-process-load-file))
    (haskell-process-load-file)))


(provide 'aph-haskell)
