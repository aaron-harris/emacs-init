;;;; The Emacs init file of Aaron Harris.
;;;; KEYBINDING SETUP
;;;;============================================================================

;;; Global Keybindings
;;;===================

;; Miscellaneous Keybindings
(global-set-key (kbd "C-+") 'flash-crosshairs)

;; Org Mode Keybindings
(global-set-key (kbd "C-c a") 'org-agenda)     ; As recommended
(global-set-key (kbd "C-c A") 'aph/org-agenda-display-smart-agenda)
(global-set-key (kbd "C-c c") 'org-capture)    ; As recommended
(global-set-key (kbd "C-c l") 'org-store-link) ; As recommended
(global-set-key (kbd "C-c j") 'org-clock-goto)
(global-set-key (kbd "C-c o") 'org-clock-out)
(global-set-key (kbd "C-c q") 'org-clock-cancel)
(global-set-key (kbd "C-c x") 'org-clock-in-last)
