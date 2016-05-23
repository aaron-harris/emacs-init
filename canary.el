;;; canary.el --- Tools to inspect Emacs state       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: lisp tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module contains functions that help an Elisp programmer
;; inspect the current state of Emacs in one way or another.
;;
;; Functions included are as follows:
;;
;; `canary':
;;
;;     This function just prints a message including its argument
;;     list.  This is most useful when used with advice; just install
;;     it as :before advice on a function of interest, and `canary'
;;     will tell you what arguments that function is called with.
;;
;; `canary-hooks':
;;
;;     This command executes another command and logs all hooks run in
;;     the process.  It is useful when a command is triggering
;;     anomalous behavior, but you don't know why.  Alternatively, it
;;     can be used to figure out what hooks exist that you can attach
;;     new code to.

;;; Code:

(eval-when-compile (require 'vizier))

;;;###autoload
(defun canary (&rest args)
  "Print a message containing ARGS."
  (message "Canary called with args %s" args))

;;;###autoload
(defun canary-hooks (command &optional verbose)
  "Call COMMAND, reporting every hook run in the process.
Interactively, prompt for a command to execute.

Return a list of the hooks run, in the order they were run.
Interactively, or with optional argument VERBOSE, also print a
message listing the hooks."
  (interactive "CCommand to log hooks: \np") 
  (let* ((log     nil)
         (logger (lambda (&rest hooks) 
                   (setq log (append log hooks nil)))))
    (vizier-with-advice
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

(provide 'canary)
;;; canary.el ends here
