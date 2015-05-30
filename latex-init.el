;;;; The Emacs inits file of Aaron Harris.
;;;; LATEX CONFIGURATION
;;;;============================================================================

;; Update style info on save, and on load if necessary.
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Use master files.
(setq-default TeX-master nil)

;;; Preview Settings
;;;=================
(setq preview-image-type 'dvipng)
(setq preview-preserve-counters t)
(setq preview-auto-cache-preamble t)

;;; Compilation and Viewer Settings
;;;================================

;; Compile to PDF by default.
(setq TeX-PDF-mode t)

;; Use Emacs to view PDF output.
(defun aph/LaTeX-use-emacs-as-viewer ()
  "Register Emacs as a PDF viewer in the variables
`TeX-view-program-selection' and `TeX-view-program-list'.
Intended for use with `LaTeX-mode-hook'."
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Emacs"))
  (add-to-list 'TeX-view-program-list
               '("Emacs" "emacsclient -n %o")))

(add-hook 'LaTeX-mode-hook 'aph/LaTeX-use-emacs-as-viewer)

;;; Docview Settings
;;;=================

;; Update the PDF automatically on changes.
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; Increase resolution for PDF images.
(setq doc-view-resolution 200)
