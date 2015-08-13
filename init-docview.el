;;; -*- lexical-binding: t -*-

;;;; The Emacs inits file of Aaron Harris.
;;;; DOCVIEW CONFIGURATION
;;;;============================================================================


;;; Program Setup
;;;==============
;; On mpc, Ghostview has a different name.
(when (eq aph/machine 'mpc)
  (setq doc-view-ghostscript-program "mgs.exe"))


;;; Miscellaneous Settings
;;;=======================
;; Increase resolution for PDF images.
(setq doc-view-resolution 200) 

(provide 'init-docview)
