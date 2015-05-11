;;;; The Emacs init file of Aaron Harris.
;;;; CLOJURE CONFIGURATION
;;;;============================================================================

;; Don't switch automatically to the REPL on connection.
(setq cider-repl-pop-to-buffer-on-connect nil)

;; Do not show stacktrace buffer for errors inside the REPL,
;; and don't auto-focus the stacktrace buffer when it is shown.
(setq cider-show-error-buffer 'except-in-repl)
(setq cider-auto-select-error-buffer nil)

;; Log nrepl messages (usually disabled).
;(setq nrepl-log-messages t)

