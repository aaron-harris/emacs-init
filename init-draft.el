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


;;; Make capture occur in new frame
;;;==================================
(defvar aph/org-capture-in-popout-frame t
  "If non-nil, `org-capture' does its work in a new frame.")

;; What we need to do:
;; - Make `org-capture-select-template' use a pop-up frame,
;;   and then keep it around.
;; --> `org-capture-select-template' delegates to
;;     `org-switch-to-buffer-other-window', so that's what we need to
;;     advise.
;; - Probably need to fix the value stored as :return-to-wconf.
;; - Make `org-capture-fill-template' reuse the same capture frame.

(defun aph/org-switch-to-buffer-in-capture-frame (&rest args)
  "Advice to support `aph/org-capture-in-popout-frame'.
When this variable is non-nil, override the usual behavior of
`org-switch-to-buffer-other-window' and open the buffer described
by ARGS in a new frame.

Otherwise, return nil to return control to
`org-switch-to-buffer-other-window'."
  (when aph/org-capture-in-popout-frame
    (apply #'switch-to-buffer-other-frame args)))

(advice-add #'org-switch-to-buffer-other-window
            :before-until
            #'aph/org-switch-to-buffer-in-capture-frame)

;; To clean up WIP code:
(advice-remove #'org-switch-to-buffer-other-window 
               #'aph/org-switch-to-buffer-in-capture-frame)


;;; Region Highlighting
;;;====================
;; This is the appropriate line to add, but we need to refactor our
;; theme code a bit before we have a good place to put it.
(set-face-attribute 'region nil :background "#666")

;; This is a local redefinition of the theme toggler that handles the
;; addition until we can refactor and make this permanent.
(defun aph/theme-night-toggle ()
  "Toggle between themes `aph/theme-day' and `aph/theme-night'.

If neither of these themes is currently active, load `aph/theme-night'." 
  (interactive)
  (if (custom-theme-enabled-p aph/theme-night)
      (progn
        (disable-theme aph/theme-night)
        (load-theme aph/theme-day :noconfirm))
    (disable-theme aph/theme-day)
    (load-theme aph/theme-night :noconfirm))
  (set-face-attribute 'region nil :background "#666"))
