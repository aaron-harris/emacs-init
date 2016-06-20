;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; WINDOW EXTENSIONS
;;;;============================================================================

;; Extensions to the `window' module.
(require 'hydra)


;;; Extensions to `other-window'
;;;=============================
(defun aph/other-window-backward (count &optional all-frames)
  "As `other-window' but reversed."
  (interactive "p")
  (other-window (- count) all-frames))


;;; Quit Help Windows
;;;==================

;; This variable is autoloaded to enable the easy addition of help
;; buffer types during initialization.

;;;###autoload
(defvar aph/help-window-names
  '(
    ;; Ubiquitous help buffers
    "*Help*"
    "*Apropos*"
    "*Messages*"
    "*Completions*"
    ;; Other general buffers
    "*Command History*"
    "*Compile-Log*"
    "*disabled command*")
  "Names of buffers that `aph/quit-help-windows' should quit.")

(defun aph/quit-help-windows (&optional kill frame)
  "Quit all windows with help-like buffers.

Call `quit-windows-on' for every buffer named in
`aph/help-windows-name'.  The optional parameters KILL and FRAME
are just as in `quit-windows-on', except FRAME defaults to t (so
that only windows on the selected frame are considered).

Note that a nil value for FRAME cannot be distinguished from an
omitted parameter and will be ignored; use some other value if
you want to quit windows on all frames."
  (interactive)
  (let ((frame (or frame t)))
    (dolist (buffer aph/help-window-names)
      (ignore-errors
        (quit-windows-on buffer kill frame)))))


;;; Scrolling Commands
;;;===================
(defhydra aph/hydra-scroll-other (:color pink)
  "Scroll other"
  ("C-v" scroll-other-window      "fwd")
  ("M-v" (scroll-other-window '-) "back") 
  ("C-g" nil                      "quit" :color blue))


(provide 'aph-window)
