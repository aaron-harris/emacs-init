;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; SILENCE
;;;;============================================================================

;; This module implements targeted message suppression.


;;; Configuration Variables
;;;========================
(defvar aph/silence-list nil
  "A list identifying messages to be silenced.

Valid entries are strings and functions.

A string entry is interpreted as a regexp; messages containing a match
will be silenced.  Note that the entire message doesn't have to
match.

A function entry is called for each message, with the same args
as the triggering `message' call, and the message is silenced if
the function returns non-nil.

See `aph/silence-advice' for more details.")

(defvar aph/silence-enabled nil
  "If non-nil, some messages will be silenced.

To control which messages are silenced, see `aph/silence-list'.
See `aph/silence-advice' for more details.")


;;; Message Suppression
;;;====================
(defun aph/silence-message-p (msg)
  "Return non-nil if MSG should be silenced per `aph/silence-list'.
If `aph/silence-enabled' is nil, return nil for any MSG.  See
`aph/silence-advice' for more details.

MSG should be a string, already formatted, just as it would
appear in the *Messages* buffer."
  (require 'dash)                       ; For `-some-p'
  (when aph/silence-enabled
    (-some-p
     (lambda (elt)
       (cond
        ((stringp elt)    (string-match-p elt msg))
        ((functionp elt)  (funcall elt msg))
        :else             (error "Invalid entry %s in `aph/silence-list'" elt)))
     aph/silence-list)))

(defun aph/silence-advice (fun &rest args)
  "Advice to enforce `aph/silence-enabled'.

If `aph/silence-enabled' is nil, do nothing.

Otherwise, consult `aph/silence-list' to see whether the message
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
     ((null msg)                   (apply fun args))
     ((equal (car args) "")        (apply fun args))
     ((aph/silence-message-p msg)  msg)
     (:else                        (apply fun args)))))

;; Note that, with the default settings, this advice does nothing.
(advice-add #'message :around #'aph/silence-advice)

(defmacro aph/silence (msg-list &rest body)
  "Execute BODY silencing messages matching MSG-LIST.

Here MSG-LIST is a list of the same format as `aph/silence-list'.
Its elements will be added to `aph/silence-list' for the duration
of BODY, and `aph/silence-enabled' will be treated as non-nil.

This is accomplished by advising `message'.  As `message' is a
primitive, not all messages can be silenced; calls from C code
may avoid being silenced."
  (declare (debug  ((&rest &or stringp function-form) body))
           (indent 1))
  `(let ((aph/silence-list (append aph/silence-list
                                   (list ,@msg-list)))
         (aph/silence-enabled t))
     ,@body))


;;; Load Message Suppression
;;;=========================
(defmacro aph/silence-loading (&rest body)
  "Execute BODY silencing `load' messages.

Note that the mechanism used is unrelated to that used by
`aph/silence'.  Instead, we advise `load'.  As `load' is also a
primitive, the same caveat regarding C calls applies."
  (declare (debug t)
           (indent 0))
  `(aph/with-advice
       ((:genname
         #'load
         :filter-args
         (-lambda ((file noerror nomessage nosuffix must-suffix))
           (list file noerror t nosuffix must-suffix))))
     ,@body))


(provide 'aph-silence)
