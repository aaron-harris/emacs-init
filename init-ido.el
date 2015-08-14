;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; IDO MODE CONFIGURATION
;;;;============================================================================


;;; Basic Settings
;;;===============
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-auto-merge-work-directories-length -1) ; Disable auto-merge


;;; Extensions
;;;===========
;; If more than one extension appears on a line, it means that I
;; expect files of those extensions might appear together, and I care
;; which order they're sorted in.  Otherwise, I just care about
;; sorting relative to the t symbol, which represents files with
;; extensions not appearing in this list.
(setq ido-file-extensions-order '(".ahk"
                                  ".clj"
                                  ".el"
                                  ".org"
                                  ".tex" ".pdf"
                                  ".txt"
                                  t
                                  ".log"))

;; Adding some extensions to ignore entirely, mostly side-effects of
;; LaTeX compilation.
(nconc completion-ignored-extensions
       '(".tex.swp" "_.log" ".prv/" "_.tex" ".rip")) 


;;; Enabling Ido
;;;=============
(ido-mode 1)
(ido-everywhere 1)

(provide 'init-ido)


