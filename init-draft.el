;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; DRAFT AREA
;;;;============================================================================

;; This file is for drafting new portions of the init files. It is not
;; automatically loaded on initialization.


;;; `hippie-unexpand'
;;;==================
(define-key read-expression-map [(tab)] 'hippie-expand)

(defun hippie-unexpand ()
  (interactive)
  (hippie-expand -1))

(define-key read-expression-map [(shift tab)] 'hippie-unexpand)


;;; Elfeed open browser links in `eww'
;;;===================================
;; This should replace binding at `C-c C-o', but perhaps not until I
;; can sort out Org-mode links.  It will also probably be used as a
;; subroutine in sorting out Elfeed.
(defun aph/browse-url (external url &rest args)
  "Browse URL in `eww', or in an external browser.

If a prefix argument is supplied, browse URL in an external
browser; otherwise, use `eww'.

Interactively, prompt the user for URL, using any URL at point as
a default."
  (interactive (cons current-prefix-arg
                     (browse-url-interactive-arg "URL: ")))
  (if external
      (apply #'browse-url (cons url args))
    (eww-browse-url url)))
