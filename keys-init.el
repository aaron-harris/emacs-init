;;;; The Emacs init file of Aaron Harris.
;;;; GLOBAL KEYBINDINGS
;;;;============================================================================

;; Miscellaneous Keybindings
(define-key global-map (kbd "C-+") 'flash-crosshairs)

;; Org Mode Keybindings
(define-key global-map "\C-cl" 'org-store-link)   ; As recommended
(define-key global-map "\C-ca" 'org-agenda)       ; As recommended
(define-key global-map "\C-cc" 'org-capture)      ; As recommended
(define-key global-map "\C-cj" 'org-clock-goto)
(define-key global-map "\C-cq" 'org-clock-cancel)
(define-key global-map "\C-co" 'org-clock-out)
(define-key global-map "\C-cx" 'org-clock-in-last)
