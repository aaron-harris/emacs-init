;;;; The Emacs init file of Aaron Harris.
;;;; KEYBINDING SETUP
;;;;============================================================================

(require 'aph-functions)                ; For `aph/kill-active-buffer'
(require 'aph-functions-org)            ; For spinner commands.


;;; Keybinding Functions
;;;=====================
(defun aph/define-keys (keymap bind-list)
  "For each (KEY . DEF) in BIND-LIST, bind KEY to DEF in KEYMAP.

See `define-key' for more information. Unlike `define-key', we
also accept for KEY any lisp code that evaluates to a valid key
sequence (e.g., a quoted call to `kbd')."
  (dolist (binding bind-list)
    (define-key keymap (eval (car binding)) (eval (cdr binding))))) 

(defun aph/global-set-key-safely (key command &optional nomsg-on-rebind)
  "As `global-set-key', but check if COMMAND is defined first.

If COMMAND is defined, proceed to bind it to KEY. If it isn't,
print a message to that effect and do not bind it.

Unless the optional parameter NOMSG-ON-REBIND is supplied, also
print a message if we are overwriting an existing binding for
KEY. In this case, proceed with the rebinding in any case. 

If the binding succeeds, return COMMAND. Otherwise return nil." 
  (catch 'esc
    (let ((old-binding (global-key-binding key)))
      (if (fboundp command)
          (global-set-key key command)
        (message "Cannot bind #'%s to %s: Command not defined"
                 command (key-description key))
        (throw 'esc nil))
      (unless (or nomsg-on-rebind
                  (not old-binding)
                  (eq old-binding command))
        (message "Warning: Binding #'%s to %s overwrites existing binding #'%s"
                 command (key-description key) old-binding))
      command)))


;;; Global Keybindings
;;;=================== 
;; Removing Unnecessary Bindings
(global-unset-key (kbd "C-z"))          ; I don't need minimization on a key.

;; Miscellaneous Keybindings 
(aph/global-set-key-safely (kbd "C-c M-c") #'cider-connect) 
(aph/global-set-key-safely (kbd "C-x M-l") #'find-library)
(aph/global-set-key-safely (kbd "C-+")     #'flash-crosshairs)
(aph/global-set-key-safely (kbd "M-/")     #'hippie-expand) ; TODO - Change to remap
(aph/global-set-key-safely (kbd "C-x C-b") #'ibuffer) ; TODO - Change to remap
(aph/global-set-key-safely (kbd "C-S-o")   #'join-line)
(aph/global-set-key-safely (kbd "C-x k")   #'aph/kill-active-buffer) ; TODO - Change to remap
(aph/global-set-key-safely (kbd "<C-tab>") #'other-window)
(aph/global-set-key-safely (kbd "C-:")     #'pp-eval-expression)
(aph/global-set-key-safely (kbd "C-c n")   #'aph/theme-night-toggle)

;; Recommended global Org-Mode keybindings
(aph/global-set-key-safely (kbd "C-c a") #'org-agenda)
(aph/global-set-key-safely (kbd "C-c c") #'org-capture)
(aph/global-set-key-safely (kbd "C-c l") #'org-store-link)

;; Other global keybindings for Org-Mode
(aph/global-set-key-safely (kbd "<f1>")
                           #'aph/org-agenda-display-smart-agenda :rebind)
(aph/global-set-key-safely (kbd "C-c j") #'org-clock-goto)
(aph/global-set-key-safely (kbd "C-c o") #'org-clock-out)
(aph/global-set-key-safely (kbd "C-c q") #'org-clock-cancel)
(aph/global-set-key-safely (kbd "C-c x") #'org-clock-in-last)


;;; Mode-Specific Keybindings
;;;==========================
;; Eww
(eval-after-load 'eww
  '(define-key eww-mode-map (kbd "S-<tab>") #'shr-previous-link))

;; Org Mode
(eval-after-load 'org
  '(progn
    (define-key org-mode-map (kbd "C-z s")   #'aph/org-spin-basic)
    (define-key org-mode-map (kbd "C-z C-s") #'aph/org-spin-weighted)))

;; Smartparens
(eval-after-load 'smartparens
  '(let ((keymap smartparens-mode-map))
     ;; Movement Commands
     (define-key keymap (kbd "C-M-b")           #'sp-backward-sexp)
     (define-key keymap (kbd "C-M-f")           #'sp-forward-sexp)
     (define-key keymap (kbd "C-M-u")           #'sp-backward-up-sexp)
     (define-key keymap (kbd "C-M-d")           #'sp-down-sexp)
     (define-key keymap (kbd "M-B")             #'sp-backward-symbol)
     (define-key keymap (kbd "M-F")             #'sp-forward-symbol)
     ;; Selection Commands
     (define-key keymap (kbd "C-]")             #'sp-select-next-thing-exchange)
     (define-key keymap (kbd "C-M-]")           #'sp-select-previous-thing)
     ;; Barf and Slurp Commands
     (define-key keymap (kbd "C-<left>")        #'sp-forward-barf-sexp)
     (define-key keymap (kbd "C-<right>")       #'sp-forward-slurp-sexp)
     (define-key keymap (kbd "C-S-<left>")      #'sp-backward-slurp-sexp)
     (define-key keymap (kbd "C-S-<right>")     #'sp-backward-barf-sexp)
     ;; Kill and Copy Commands
     (define-key keymap (kbd "C-M-k")           #'sp-kill-sexp)
     (define-key keymap (kbd "C-M-w")           #'sp-copy-sexp)
     (define-key keymap (kbd "C-S-<backspace>") #'sp-splice-sexp-killing-around)
     ;; Unwrap and Splice Commands
     (define-key keymap (kbd "C-<delete>")      #'sp-unwrap-sexp)
     (define-key keymap (kbd "C-<backspace>")   #'sp-backward-unwrap-sexp)
     (define-key keymap (kbd "M-D")             #'sp-splice-sexp)
     ;; Reconfiguration Commands
     (define-key keymap (kbd "C-M-t")           #'sp-transpose-sexp)
     (define-key keymap (kbd "M-)")             #'sp-up-sexp)
     ;; Other Commands
     (define-key keymap (kbd "C-x n )")         #'sp-narrow-to-sexp)
     ;; Strict Smartparens Commands
     ;;(define-key smartparens-strict-mode-map (kbd "M-q") #'sp-indent-defun)
     (define-key smartparens-strict-mode-map (kbd ")")   #'sp-up-sexp)))


;;; Machine-Specific Keybindings
;;;=============================
(when (eq aph/machine 'mpc)
  (aph/require-softly 'aph-geog)
  (aph/global-set-key-safely (kbd "C-x C-y") #'aph/yank-access-inline))

(provide 'init-keys)
