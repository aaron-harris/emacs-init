;;; -*- lexical-binding: t -*-

;;;; The Emacs inits file of Aaron Harris.
;;;; LATEX EXTENSIONS
;;;;============================================================================


;;; Emacs as Viewer
;;;================
(defun aph/LaTeX-use-emacs-as-viewer ()
  "Register Emacs as a PDF viewer.

Sets up the variables `TeX-view-program-selection' and
`TeX-view-program-list'. Intended for use in `LaTeX-mode-hook'."
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Emacs"))
  (add-to-list 'TeX-view-program-list
               '("Emacs" "emacsclient -n %o")))

(provide 'aph-latex)
