;;; silence.el --- Silence unwanted messages         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

;; Dependencies: `seq', `vizier' (load message suppression only),
;;   `validate' (optional)
;; Advised functions from other packages:
;;   built-in: `message'

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

;; This module provides tools to suppress unwanted messages from
;; appearing in the *Messages* buffer.  Note that this is implemented
;; through advice on `message'.  This runs contrary to the admonition
;; never to advise a primitive, but this seems to be the only way to
;; accomplish this, and does not seem to cause any unwanted effects.
;; Nevertheless, use at your own risk!
;;
;; Three silencing mechanisms are provided.  Each can be used in a
;; different way:
;;
;; First, there is global message suppression.  To use this form,
;; require this module, set the variable `silence-enabled-p' to a
;; non-nil value, and populate the list `silence-list'.  Members of
;; this list should be either regexps matching the messages to
;; silence, or functions.  Functions are called with a single string
;; argument (the message under consideration); if the function returns
;; non-nil, the message will be suppressed.
;;
;; Next, there is the `silence' macro.  This macro accepts a list of
;; the same format as `silence-list' and suppresses all matching
;; messages (in addition to those matching the actual contents of
;; `silence-list') for the duration of its body.  This can be used to
;; suppress messages within a specific block of code.
;;
;; Finally, there is the macro `silence-loading'.  This silences
;; messages generated in its body by the `load' command.  Note that
;; this uses an entirely different mechanism than the other two forms
;; of suppression; it temporarily advises the `load' function to
;; supply the argument causing it to suppress its own messages.
;;
;; Both `silence' and `silence-loading' are autoloaded, so there is no
;; need to require this module explicitly unless you are using global
;; message suppression.

;;; Code:

(require 'seq)                          ; For `seq-some'
(eval-when-compile (require 'dash))     ; For `-lambda'


;;;; User Options
;;===============
(defgroup silence nil
  "Silence unwanted messages."
  :prefix "silence-"
  :link '(emacs-commentary-link "silence")
  :group 'convenience)

(defcustom silence-enabled-p nil
  "If non-nil, some messages will be silenced.

To control which messages are silenced, see `silence-list'.
See `silence-advice' for more details."
  :type 'boolean)

(defcustom silence-list nil
  "A list identifying messages to be silenced.

Valid entries are strings and functions.

A string entry is interpreted as a regexp; messages containing a match
will be silenced.  Note that the entire message doesn't have to
match.

A function entry is called for each message, with the same args
as the triggering `message' call, and the message is silenced if
the function returns non-nil.

See `silence-advice' for more details."
  :type '(repeat (choice string function)))


;;;; General Message Suppression
;;==============================
(defun silence-message-p (msg)
  "Return non-nil if MSG should be silenced per `silence-list'.
If `silence-enabled-p' is nil, return nil for any MSG.  See
`silence-advice' for more details.

MSG should be a string, already formatted, just as it would
appear in the *Messages* buffer."
  (when (require 'validate nil :noerror)
    (validate-variable 'silence-list))
  (when silence-enabled-p
    (seq-some
     (lambda (elt)
       (cond
        ((stringp elt)    (string-match-p elt msg))
        ((functionp elt)  (funcall elt msg))
        :else             (error "Invalid entry %s in `silence-list'" elt)))
     silence-list)))

(defun silence-advice (fun &rest args)
  "Advice to enforce `silence-enabled-p'.

If `silence-enabled-p' is nil, do nothing.

Otherwise, consult `silence-list' to see whether the message
defined by ARGS should be silenced (where ARGS is the arglist for
the pending `message' command.

If a message is silenced, `format' is called instead of
`message'.  Thus, the return value is unchanged, but no message
is shown in the *Messages* buffer.

This function is intended as :around advice for `message'.  As
`message' is a primitive, this is not ideal, but there doesn't
seem to be any way to achieve this kind of message suppression
otherwise.  Because `message' is a primitive, not all messages
can be silenced; calls from C code may avoid being silenced." 
  (let ((msg  (and (car args) (apply #'format args))))
    (cond
     ((null msg)               (apply fun args))
     ((equal (car args) "")    (apply fun args))
     ((silence-message-p msg)  msg)
     (:else                    (apply fun args)))))

;; Note that, with the default settings, this advice does nothing.
(advice-add 'message :around #'silence-advice)


;;;; Local Message Suppression
;;============================
;;;###autoload
(defmacro silence (msg-list &rest body)
  "Execute BODY silencing messages matching MSG-LIST.

Here MSG-LIST is a list of the same format as `silence-list'.
Its elements will be added to `silence-list' for the duration
of BODY, and `silence-enabled' will be treated as non-nil.

This is accomplished by advising `message'.  As `message' is a
primitive, not all messages can be silenced; calls from C code
may avoid being silenced."
  (declare (debug  ((&rest &or stringp function-form) body))
           (indent 1))
  `(let ((silence-list (append silence-list
                               (list ,@msg-list)))
         (silence-enabled-p t))
     ,@body))


;;;; Load Message Suppression
;;===========================
;;;###autoload
(defmacro silence-loading (&rest body)
  "Execute BODY silencing `load' messages.

Note that the mechanism used is unrelated to that used by the
`silence' macro.  Instead, we advise `load'.  As `load' is also a
primitive, the same caveat regarding C calls applies."
  (declare (debug t)
           (indent 0))
  (require 'vizier)                     ; For `vizier-with-advice'
  `(vizier-with-advice
       ((:genname
         #'load
         :filter-args
         (-lambda ((file noerror nomessage nosuffix must-suffix))
           (list file noerror t nosuffix must-suffix))))
     ,@body))


;;;; Unloading
;;============
(defun silence-unload-function ()
  "Undo changes made to support `silence' module.

More specifically, remove advice `silence-advice' from
`message'."
  (advice-remove 'message #'silence-advice))

(provide 'silence)
;;; silence.el ends here
