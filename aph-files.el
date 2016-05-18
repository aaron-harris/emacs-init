;;; aph-files.el --- Extensions for `files` module   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris;;; -*- lexical-binding: t -*- <meerwolf@gmail.com>
;; Keywords: files

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

;; Miscellaneous functions extending those in the `files' module built
;; into Emacs.

;;; Code:


;;; Killing Buffers
;;;================
(defun aph/kill-active-buffer (&optional choose)
  "Kill the active buffer.

With a prefix argument, choose the buffer to kill (as the
standard `kill-buffer')."
  (interactive "P")
  (if choose
      (call-interactively #'kill-buffer)
    (kill-buffer)))

(defun aph/kill-active-buffer-delete-file (&optional choose noconfirm)
  "As `aph/kill-active-buffer', and delete associated file.

Ask the user for confirmation before deleting the file, unless
the optional parameter NOCONFIRM is non-nil."
  (interactive "P")
  (let ((buf  (current-buffer))
        (file (buffer-file-name)))
    (aph/kill-active-buffer choose)
    (when (and file
               (not (buffer-live-p buf))
               (y-or-n-p (format "Really delete file %s? " file)))
      (delete-file file))))

(defun aph/kill-buffer-nowarn (&optional buffer-or-name)
  "Kill buffer specified by BUFFER-OR-NAME, without asking.

As `kill-buffer', but do not ask for confirmation before killing
a modified buffer.  Also bypass all the functions named in
`kill-buffer-query-functions'.  (With the default value, this
will bypass confirmation before killing buffers with running
processes.)"
  (setq buffer-or-name (or buffer-or-name (current-buffer)))
  (let ((buffer (get-buffer buffer-or-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (restore-buffer-modified-p nil)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(defun aph/kill-buffer-if-any (buffer-or-name &optional nowarn)
  "Kill BUFFER-OR-NAME if it exists.

As `kill-buffer', except no error is signaled if BUFFER-OR-NAME
is a buffer that has already been killed, or the name of a buffer
that does not exist.

If the optional parameter NOWARN is non-nil, also bypass all
confirmations as `aph/kill-buffer-nowarn'."
  (when (get-buffer buffer-or-name)
    (if nowarn (aph/kill-buffer-nowarn buffer-or-name)
      (kill-buffer buffer-or-name))))


;;; Extensions to `save-buffers-kill-emacs'
;;;========================================
(defun aph/delete-frame-or-exit (&optional arg)
  "Delete this frame.  With only one frame, exit Emacs.

When called from a non-emacsclient frame and there is more than
one visible frame, delete the current frame (see `delete-frame').
Otherwise, call `save-buffers-kill-terminal'.

With one prefix ARG (`C-u'), instead kill Emacs entirely with
`save-buffers-kill-emacs'.

With two prefix args (`C-u C-u'), save all buffers and kill
Emacs, bypassing all normal confirmation prompts."
  (interactive "P")
  (cond
   ((equal arg '(16))                    (let ((confirm-kill-emacs nil))
                                           (save-buffers-kill-emacs arg)))
   ((equal arg '(4))                     (save-buffers-kill-emacs)) 
   ((> (length (visible-frame-list)) 1)  (delete-frame))
   (:else                                (save-buffers-kill-terminal))))

(provide 'aph-files)
;;; aph-files.el ends here
