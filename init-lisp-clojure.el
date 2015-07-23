;;;; The Emacs init file of Aaron Harris.
;;;; CLOJURE CONFIGURATION
;;;;============================================================================

;; Enable subword mode in Clojure buffers:
(add-hook 'clojure-mode-hook    #'subword-mode)
(add-hook 'cider-repl-mode-hook #'subword-mode)

;; Enable eldoc mode in Clojure buffers:
(require 'cider-eldoc)
(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook   #'eldoc-mode)

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
(defun aph/remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)

  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; Enabling the above-mentioned fix:
(add-hook 'cider-repl-mode-hook            #'aph/remove-dos-eol)
(add-hook 'cider-macroexpansion-mode-hook  #'aph/remove-dos-eol)
(add-hook 'cider-test-report-mode-hook     #'aph/remove-dos-eol)

;; Log nrepl messages (uncomment to diagnose REPL problems).
;;(setq nrepl-log-messages t)

;; Replace the `' pair with `` in Clojure mode. 
(sp-local-pair 'clojure-mode "`" "`"
               :when '(sp-in-string-p sp-in-comment-p))

(provide 'init-lisp-clojure)