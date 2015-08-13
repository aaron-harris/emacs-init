;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; KEYBINDING SETUP
;;;;============================================================================

;; Load custom keybinding functions.
(require 'aph-keys)

;; Load custom command definitions.
(aph/require-softly 'aph-functions)
(aph/require-softly 'aph-org)
(aph/require-softly 'aph-theme)
(when (eq aph/machine 'mpc)
  (aph/require-softly 'aph-mpc))


;;; General Keybindings
;;;====================
;; Removing Unnecessary Bindings
(global-unset-key (kbd "C-z"))          ; I don't need minimization on a key.

(aph/global-set-keys-safely (not :pkg)
  ;; Scrolling Commands
  ((kbd "<down>")    #'aph/scroll-up-by-line      :rebind)
  ((kbd "<up>")      #'aph/scroll-down-by-line    :rebind) 
  ;; Editing Commands
  ((kbd "C-S-o")     #'join-line) 
  ;; Completion Commands
  ((kbd "M-/")       #'hippie-expand              :rebind)
  ;; Buffer, Frame, and Window Control 
  ((kbd "C-x C-b")   #'ibuffer                    :rebind)
  ((kbd "C-x k")     #'aph/kill-active-buffer     :rebind)
  ((kbd "<C-tab>")   #'other-window)
  ((kbd "<C-S-tab>") #'aph/other-window-backwards)
  ((kbd "C-x C-c")   #'aph/delete-frame-or-exit   :rebind) 
  ;; Display Commands
  ((kbd "C-+")       #'flash-crosshairs)
  ((kbd "C-c n")     #'aph/theme-night-toggle))


;;; Smartparens Keybindings
;;;========================
(aph/define-keys-safely smartparens-mode-map 'smartparens 
  ;; Movement Commands
  ((kbd "C-M-b")           #'sp-backward-sexp)
  ((kbd "C-M-f")           #'sp-forward-sexp)
  ((kbd "C-M-u")           #'sp-backward-up-sexp)
  ((kbd "C-M-d")           #'sp-down-sexp)
  ((kbd "M-B")             #'sp-backward-symbol)
  ((kbd "M-F")             #'sp-forward-symbol)
  ;; Selection Commands
  ((kbd "C-]")             #'sp-select-next-thing-exchange)
  ((kbd "C-M-]")           #'sp-select-previous-thing)
  ;; Barf and Slurp Commands
  ((kbd "C-<left>")        #'sp-forward-barf-sexp)
  ((kbd "C-<right>")       #'sp-forward-slurp-sexp)
  ((kbd "C-S-<left>")      #'sp-backward-slurp-sexp)
  ((kbd "C-S-<right>")     #'sp-backward-barf-sexp)
  ;; Kill and Copy Commands
  ((kbd "C-M-k")           #'sp-kill-sexp)
  ((kbd "C-M-w")           #'sp-copy-sexp)
  ((kbd "C-S-<backspace>") #'sp-splice-sexp-killing-around)
  ;; Unwrap and Splice Commands
  ((kbd "C-<delete>")      #'sp-unwrap-sexp)
  ((kbd "C-<backspace>")   #'sp-backward-unwrap-sexp)
  ((kbd "M-D")             #'sp-splice-sexp)
  ;; Reconfiguration Commands
  ((kbd "C-M-t")           #'sp-transpose-sexp)
  ((kbd "M-)")             #'sp-up-sexp)
  ;; Other Commands
  ((kbd "C-x n (")         #'sp-narrow-to-sexp))

(aph/define-keys-safely smartparens-strict-mode-map 'smartparens
  ((kbd ")")   #'sp-up-sexp))


;;; Org Mode Keybindings
;;;=====================
(aph/global-set-keys-safely 'org
  ;; Agenda Commands
  ((kbd "C-c a")   #'org-agenda)
  ((kbd "<f1>")    #'aph/org-agenda-display-smart-agenda :rebind)
  ;; Capture and Refile Commands
  ((kbd "C-c c")   #'org-capture)
  ((kbd "C-c w")   #'aph/org-goto-last-refile)
  ;; Link Commands
  ((kbd "C-c l")   #'org-store-link)
  ;; Clocking Commands 
  ((kbd "C-c t j") #'org-clock-goto)
  ((kbd "C-c t o") #'org-clock-out)
  ((kbd "C-c t x") #'org-clock-cancel)
  ((kbd "C-c t r") #'org-clock-in-last))

(aph/define-keys-safely org-mode-map 'org
  ;; Clocking Commands
  ((kbd "C-c t i") #'org-clock-in) 
  ;; Spinner Commands
  ((kbd "C-z s")   #'aph/org-spin-basic)
  ((kbd "C-z C-s") #'aph/org-spin-weighted))


;;; Programming Keybindings
;;;========================
;; Elisp
(aph/global-set-keys-safely (not :pkg)
  ((kbd "C-x M-l") #'find-library)
  ((kbd "C-:")     #'pp-eval-expression))

;; This next form errors if the `eval-after-load' is removed,
;; precisely because `ielm-map' is not defined until after 'ielm is
;; loaded.  I may need to rethink my implementation of
;; `aph/define-keys-safely'.
(eval-after-load 'ielm
  '(aph/define-keys-safely ielm-map 'ielm
    ((kbd "C-c M-w") #'aph/ielm-copy-last-output)
    ((kbd "C-c C-t") #'aph/eval-expression-toggle-clean-output)))

;; Clojure and Cider
(aph/global-set-keys-safely (not :pkg)
  ((kbd "C-c M-c") #'cider-connect))


;;; Other Keybindings
;;;==================
;; Eww

;; Again, we can't strip the `eval-after-load' until we fix
;; `aph/define-keys-safely'.
(eval-after-load 'eww
  '(aph/define-keys-safely eww-mode-map 'eww
    ((kbd "S-<tab") #'shr-previous-link)))


;;; Machine-Specific Keybindings
;;;=============================
(when (eq aph/machine 'mpc) 
  (aph/global-set-key-safely (kbd "C-x C-y") #'aph/yank-access-inline))

(provide 'init-keys)
