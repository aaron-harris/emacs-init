;;;; The Emacs init file of Aaron Harris.
;;;; KEYBINDING SETUP
;;;;============================================================================

;;; Global Keybindings
;;;===================

;; Miscellaneous Keybindings
(global-set-key (kbd "C-+") #'flash-crosshairs)

;; Org Mode Keybindings
(global-set-key (kbd "C-c a") #'org-agenda)     ; As recommended
(global-set-key (kbd "C-c A") #'aph/org-agenda-display-smart-agenda)
(global-set-key (kbd "C-c c") #'org-capture)    ; As recommended
(global-set-key (kbd "C-c l") #'org-store-link) ; As recommended
(global-set-key (kbd "C-c j") #'org-clock-goto)
(global-set-key (kbd "C-c o") #'org-clock-out)
(global-set-key (kbd "C-c q") #'org-clock-cancel)
(global-set-key (kbd "C-c x") #'org-clock-in-last)

;;; Smartparens Mode Keybindings
;;;=============================

;; Smartparens
(aph/define-keys 'smartparens-mode-map
                 (append
                  ;; Movement Commands
                  '(((kbd "C-S-a") . #'sp-beginning-of-sexp)
                    ((kbd "C-S-e") . #'sp-end-of-sexp)
                    ((kbd "C-M-b") . #'sp-backward-sexp)
                    ((kbd "C-M-f") . #'sp-forward-sexp)
                    ((kbd "C-M-p") . #'sp-previous-sexp)
                    ((kbd "C-M-n") . #'sp-next-sexp)
                    ((kbd "C-M-u") . #'sp-up-sexp)
                    ((kbd "C-M-d") . #'sp-down-sexp)
                    ((kbd "M-B")   . #'sp-backward-symbol)
                    ((kbd "M-F")   . #'sp-forward-symbol))
                  ;; Selection Commands
                  '(((kbd "C-]")   . #'sp-select-next-thing-exchange)
                    ((kbd "C-M-]") . #'sp-select-next-thing-exchange))
                  ;; Barf and Slurp Commands
                  '(((kbd "C-<left>")    . #'sp-forward-barf-sexp)
                    ((kbd "C-<right>")   . #'sp-forward-slurp-sexp)
                    ((kbd "C-S-<left>")  . #'sp-backward-slurp-sexp)
                    ((kbd "C-S-<right>") . #'sp-backward-barf-sexp))

                  ;; Kill and Copy Commands
                  '(((kbd "C-M-k")           . #'sp-kill-sexp)
                    ((kbd "C-M-w")           . #'sp-copy-sexp)
                    ((kbd "C-S-<backspace>") . #'sp-splice-sexp-killing-around))

                  ;; Unwrap and Splice Commands
                  '(((kbd "C-<delete>")    . #'sp-unwrap-sexp)
                    ((kbd "C-<backspace>") . #'sp-backward-unwrap-sexp)
                    ((kbd "M-D")           . #'sp-splice-sexp))))
