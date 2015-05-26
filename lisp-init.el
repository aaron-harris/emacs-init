;;;; The Emacs init file of Aaron Harris.
;;;; LISP CONFIGURATION
;;;;============================================================================

(defvar aph/lisp-mode-hooks
  '(cider-repl-mode-hook
    clojure-mode-hook
    emacs-lisp-mode-hook
    eval-expression-minibuffer-setup-hook
    ielm-mode-hook
    lisp-interaction-mode-hook
    lisp-mode-hook)
  "A list of mode hooks for all modes in which Lisp editing
occurs. Can be used with aph/add-hook-to-all to easily add a hook
to all such modes.")

;; Enabling strict Smartparens and Rainbow Delimiters for all Lisps:
(aph/add-hook-to-all aph/lisp-mode-hooks #'smartparens-strict-mode)
(aph/add-hook-to-all aph/lisp-mode-hooks #'rainbow-delimiters-mode)

;; Have smartparens handle strings better:
(add-to-list 'sp-navigate-consider-stringlike-sexp 'lisp-mode)
(add-to-list 'sp-navigate-consider-stringlike-sexp 'clojure-mode)

;; Smartparens isn't treating the minibuffer during M-: as a lisp mode, so we
;; need to disable the single-quote pair there.
(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;(sp-pair "'" nil :actions nil :when '(aph/in-minibuffer-eval-p))

;;; Clojure-Specific Configuration
;;;===============================

;; Enable subword mode in Clojure buffers:
(add-hook 'clojure-mode-hook               #'subword-mode)
(add-hook 'cider-repl-mode-hook            #'subword-mode)

;; Don't switch automatically to the REPL on connection.
(setq cider-repl-pop-to-buffer-on-connect nil)

;; Do not show stacktrace buffer for errors inside the REPL,
;; and don't auto-focus the stacktrace buffer when it is shown.
(setq cider-show-error-buffer 'except-in-repl)
(setq cider-auto-select-error-buffer nil)

;; Output from the JVM has Windows-style newlines, so some Cider output will end
;; up including ^M characters. Telling the JVM to use Unix-style newlines is
;; apparently a bad idea, so we're just going to tell Cider to make them
;; invisible. Here's the function to do that, which we took from a comment here:
;;   https://github.com/clojure-emacs/cider/issues/474
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)

  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; Enabling the above-mentioned fix:
(add-hook 'cider-repl-mode-hook            #'remove-dos-eol)
(add-hook 'cider-macroexpansion-mode-hook  #'remove-dos-eol)
(add-hook 'cider-test-report-mode-hook     #'remove-dos-eol)

;; Log nrepl messages (uncomment to diagnose REPL problems).
;(setq nrepl-log-messages t)

