;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; DRAFT AREA
;;;;============================================================================

;; This file is for drafting new portions of the init files. It is not
;; automatically loaded on initialization.


;;; Freeing `C-[' and `C-i'
;;;========================
(define-key input-decode-map [?\C-\[] (kbd "<C-[>"))


(provide 'init-draft)
