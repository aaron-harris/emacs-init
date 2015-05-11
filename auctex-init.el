;;;; The Emacs inits file of Aaron Harris.
;;;; LATEX CONFIGURATION
;;;;============================================================================

;; Compile to PDF by default.
(setq TeX-PDF-mode t)

;; Use Emacs to view PDF output.
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list 'TeX-view-program-selection
                         '(output-pdf "Emacs"))
            (add-to-list 'TeX-view-program-list
                         '("Emacs" "emacsclient -n %o"))))

;; Update the PDF automatically on changes.
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
