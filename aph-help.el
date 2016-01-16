;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; HELP EXTENSIONS
;;;;============================================================================

;; Extensions for `help' module.


;;; Hook Logging
;;;=============
;; This function might not belong in this library, but there doesn't
;; seem to be a better place for it.
(defun aph/call-logging-hooks (command &optional verbose)
  "Call COMMAND, reporting every hook run in the process.
Interactively, prompt for a command to execute.

Return a list of the hooks run, in the order they were run.
Interactively, or with optional argument VERBOSE, also print a
message listing the hooks."
  (interactive "CCommand to log hooks: \np")
  (require 'aph-advice)                 ; For `aph/with-advice'
  (let* ((log     nil)
         (logger (lambda (&rest hooks) 
                   (setq log (append log hooks nil)))))
    (aph/with-advice
        ((#'run-hooks :before logger))
      (call-interactively command))
    (when verbose
      (message
       (if log "Hooks run during execution of %s:"
         "No hooks run during execution of %s.")
       command)
      (dolist (hook log)
        (message "> %s" hook)))
    log))

(provide 'aph-help)
