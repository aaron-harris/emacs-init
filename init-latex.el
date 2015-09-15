;;; -*- lexical-binding: t -*-

;;;; The Emacs inits file of Aaron Harris.
;;;; LATEX CONFIGURATION
;;;;============================================================================


;;; Basic Settings
;;;===============
;; Use master files.
(setq-default TeX-master nil)


;;; Caching Settings
;;;=================
;; Update style info on save, and on load if necessary.
(setq TeX-auto-save t)
(setq TeX-parse-self t) 


;;; Outline Settings
;;;=================
(add-hook 'LaTeX-mode-hook #'outline-minor-mode)


;;; Preview Settings
;;;=================
(setq preview-image-type 'dvipng)
(setq preview-preserve-counters t)
(setq preview-auto-cache-preamble t)


;;; Math Mode Settings
;;;===================
(add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

(defun aph/LaTeX-math-prime ()
  "Insert '^{\\prime}'.  For use in `LaTeX-math-list'."
  (interactive)
  (insert "^{\\prime}"))

(setq LaTeX-math-list '(("'" aph/LaTeX-math-prime)))


;;; RefTeX Setup
;;;============= 
(add-hook 'LaTeX-mode-hook #'turn-on-reftex)
(setq reftex-plug-into-auctex t)


;;; Compilation and Viewer Settings
;;;================================ 
;; Compile to PDF by default.
(setq TeX-PDF-mode t)

;; Use Emacs to view PDF output.
(defun aph/LaTeX-use-emacs-as-viewer ()
  "Register Emacs as a PDF viewer.

Sets up the variables `TeX-view-program-selection' and
`TeX-view-program-list'. Intended for use in `LaTeX-mode-hook'."
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Emacs"))
  (add-to-list 'TeX-view-program-list
               '("Emacs" "emacsclient -n %o")))

(add-hook 'LaTeX-mode-hook #'aph/LaTeX-use-emacs-as-viewer)


;;; Other Settings
;;;===============
;; Register help-type buffers for `aph/quit-help-windows'.
(eval-after-load 'aph-commands
  '(add-to-list 'aph/help-window-names "*TeX Help*"))

;; Set `fill-column' to 75 in LaTeX buffers.
(defun aph/LaTeX-mode-hook ()
  "Apply my settings for `LaTeX-mode'."
  (setq fill-column 75))
(add-hook 'LaTeX-mode-hook #'aph/LaTeX-mode-hook)

(provide 'init-latex)
