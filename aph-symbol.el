;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; SYMBOL MANIPULATION FUNCTIONS
;;;;============================================================================

;; Functions for manipulating symbols.


;;; Symbol Construction
;;;====================
(defun aph/symbol-concat (symbol suffix)
  "Return the symbol obtained from SYMBOL by appending SUFFIX.

The symbol returned is always the canonical symbol, even if
SYMBOL is uninterned.

SUFFIX should be a string."
  (intern (concat (symbol-name symbol) suffix)))


;;; Symbol Properties
;;;==================
(defun aph/symbol-prop-delq (elt symbol propname)
  "Delete ELT from list stored on SYMBOL at PROPNAME.

The deletion is accomplished via `delq', so list elements are
compared to ELT with `eq'."
  (put symbol propname
       (delq elt (get symbol propname))))

(provide 'aph-symbol)
