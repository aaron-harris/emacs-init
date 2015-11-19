;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; SYMBOL PROPERTY FUNCTIONS
;;;;============================================================================

;; Functions for working with symbol properties. 
(defun aph/symprop-delq (elt symbol propname)
  "Delete ELT from list stored on SYMBOL at PROPNAME.

The deletion is accomplished via `delq', so list elements are
compared to ELT with `eq'."
  (put symbol propname
       (delq elt (get symbol propname))))

(provide 'aph-symprop)
