;;;; The Emacs inits file of Aaron Harris.
;;;; LATEX CONFIGURATION
;;;;============================================================================

;; Compile to PDF by default.
(setq TeX-PDF-mode t)

;; Use Emacs to view PDF output.
(defun aph/LaTeX-use-emacs-as-viewer ()
  "Register Emacs as a PDF viewer in the variables
`TeX-view-program-selection' and `TeX-view-program-list'. Intended
for use with `LaTeX-mode-hook'."
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Emacs"))
  (add-to-list 'TeX-view-program-list
               '("Emacs" "emacsclient -n %o")))

(add-hook 'LaTeX-mode-hook 'aph/LaTeX-use-emacs-as-viewer)

;; Update the PDF automatically on changes.
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; Increase resolution for PDF images.
(setq doc-view-resolution 200)
