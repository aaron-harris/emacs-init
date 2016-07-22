;;; bfw.el --- Buffer, frame, and window library     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: extensions

;; Dependencies: `seq'

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

;; This module is a library containing functions associated with
;; buffers, frames, and windows.
;;
;; Included functions are as follows.  See individual function
;; docstrings for more detailed information.

;;;; Buffer functions:
;;--------------------
;;
;; `bfw-get-buffers-by-regexp':
;;
;;     Return a list of all active buffers whose names match the
;;     specified regexp.
;;
;; `bfw-kill-buffer-if-any':
;;
;;     Kill the specified buffer if it exists, but don't signal an
;;     error if it doesn't.
;;
;; `bfw-kill-buffer-nowarn':
;;
;;     Kill the specified buffer, bypassing all normal protections.

;;;; Frame functions and macros:
;;------------------------------
;;
;; `bfw-get-frame-by-name':
;;
;;     Given a string, return any frame with that string as its name.
;;
;;
;; `bfw-save-frame-excursion':
;;
;;     A version of `save-window-excursion' for frame configurations.

;;;; Window functions:
;;--------------------
;;
;; `bfw-get-nth-window-with-predicate':
;;
;;     This is a version of `get-window-with-predicate' that takes a
;;     numerical argument.
;;
;; `bfw-get-nth-window-not-dedicated':
;;
;;     This is a non-interactive version of `other-window' that skips
;;     windows dedicated to their buffers.

;;;; Buffer display functions:
;;----------------------------
;;
;; All of these functions are designed for use with
;; `buffer-display-alist'.
;;
;; `bfw-display-buffer-in-named-frame':
;;
;;     A variant of `display-buffer-pop-up-frame' that names the
;;     resulting frame and re-uses any existing frame that already has
;;     the specified name.
;;
;; `bfw-display-buffer-in-subwindow':
;;
;;     This function is similar to `display-buffer-below-selected',
;;     but it never reuses an existing window, and the resulting
;;     window has a small fixed height.


;;; Code:

(require 'seq)


;;;; Buffers
;;==========
(defun bfw-get-buffers-by-regexp (regexp)
  "Return a list of all buffers whose names match REGEXP." 
  (seq-filter (lambda (buf)
                (string-match-p regexp (buffer-name buf)))
              (buffer-list)))

(defun bfw-get-buffer-for-file (file)
  "Return the buffer visiting FILE, if any; else, nil."
  (seq-find (lambda (buf)
              (equal file (buffer-file-name buf)))
            (buffer-list)))

(defun bfw-kill-buffer-nowarn (&optional buffer-or-name)
  "Kill buffer specified by BUFFER-OR-NAME, without asking.

As `kill-buffer', but do not ask for confirmation before killing
a modified buffer.  Also bypass all the functions named in
`kill-buffer-query-functions'.  (With the default value, this
will bypass confirmation before killing buffers with running
processes.)"
  (let* ((buffer-or-name  (or buffer-or-name (current-buffer)))
         (buffer          (get-buffer buffer-or-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (restore-buffer-modified-p nil)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(defun bfw-kill-buffer-if-any (buffer-or-name &optional nowarn)
  "Kill BUFFER-OR-NAME if it exists.

As `kill-buffer', except no error is signaled if BUFFER-OR-NAME
is a buffer that has already been killed, or the name of a buffer
that does not exist.

If the optional parameter NOWARN is non-nil, also bypass all
confirmations as `bfw-kill-buffer-nowarn'."
  (when (get-buffer buffer-or-name)
    (if nowarn (bfw-kill-buffer-nowarn buffer-or-name)
      (kill-buffer buffer-or-name))))


;;;; Frames
;;=========
(defun bfw-get-frame-by-name (fname)
  "If there is a frame named FNAME, return it, else nil."
  (seq-find (lambda (frame)
              (equal fname (frame-parameter frame 'name)))
            (frame-list)))

(defmacro bfw-save-frame-excursion (&rest body)
  "As `save-window-excursion', but for frame configurations."
  (declare (indent 0) (debug t))
  (let ((frame-config (make-symbol "frame-config")))
    `(let ((,frame-config (current-frame-configuration)))
       (unwind-protect (progn ,@body)
         (set-frame-configuration ,frame-config)))))


;;;; Windows
;;==========
(defun bfw-get-nth-window-with-predicate
    (n predicate &optional minibuf all-frames default)
  "As `get-window-with-predicate', but return the Nth success.

If N is positive, return the Nth window forward in the cyclic
ordering of windows that is not dedicated to its current buffer.

If N is negative, instead return the (- N)th window backwards.

If N is zero, return selected window if it is not dedicated and
DEFAULT otherwise (or nil if DEFAULT is not supplied).

The parameters MINIBUF, ALL-FRAMES, and DEFAULT have the same
meanings as in `get-window-with-predicate'."
  (cond
   ((null (get-window-with-predicate predicate minibuf all-frames))
    default)
   ((= n 0)
    (if (funcall predicate (selected-window))
        (selected-window)
      default))
   (t
    (let*  ((cycler  (cond
                      ((> n 0) #'next-window)
                      ((< n 0) #'previous-window)))
            (cursor  (selected-window))
            (i        (abs n)))
      (while (> i 0)
        (setq cursor (funcall cycler cursor minibuf all-frames))
        (when (funcall predicate cursor)
          (setq i (1- i))))
      cursor))))

(defun bfw-get-nth-window-not-dedicated
    (n &optional minibuf all-frames default)
  "Return the Nth window forward that is not dedicated.

If N is positive, return the Nth window forward in the cyclic
ordering of windows that is not dedicated to its current buffer.

If N is negative, instead return the (- N)th window backwards.

If N is zero, return selected window if it is not dedicated and
DEFAULT otherwise (or nil if DEFAULT is not supplied).

The parameters MINIBUF, ALL-FRAMES, and DEFAULT have the same
meanings as in `get-window-with-predicate'."
  (bfw-get-nth-window-with-predicate
   n
   (lambda (win) (not (window-dedicated-p win)))
   minibuf all-frames default))


;;;; Buffer Display Functions
;;===========================
(defun bfw-display-buffer-in-named-frame (buffer alist)
  "Display BUFFER in frame with specific name.
The name to use is the value associated with the 'named-frame key
in ALIST.  If a frame with that name already exists, use it.
Otherwise, call `display-buffer-pop-up-frame' to create it.

If ALIST does not contain the key 'named-frame, use the name of
BUFFER."
  (let* ((fname  (or (cdr (assq 'named-frame alist))
                     (buffer-name buffer)))
         (frame  (bfw-get-frame-by-name fname)))
    (if frame
        (window--display-buffer buffer
                                (frame-selected-window frame)
                                'reuse)
      (display-buffer-pop-up-frame
       buffer
       (cons `(pop-up-frame-parameters (name . ,fname)) alist)))))

(defun bfw-display-buffer-in-subwindow (buffer alist)
  "Display BUFFER in a small split window.

Split the current window, with the new window being small (5
lines high) and below the original, and display BUFFER there.

The second parameter ALIST is ignored and exists only so this
function can be used in `display-buffer-alist'."
  (let ((new-win (split-window (selected-window) -5 'below)))
    (set-window-buffer new-win buffer)
    new-win))

(provide 'bfw)
;;; bfw.el ends here
