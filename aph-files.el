;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; FILE-HANDLING EXTENSIONS
;;;;============================================================================

;; Extensions for `files' package.


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

(defun aph/kill-buffer-nowarn (&optional buffer-or-name)
  "Kill buffer specified by BUFFER-OR-NAME, without asking.

As `kill-buffer', but do not ask for confirmation before killing
a modified buffer.  Also bypass all the functions named in
`kill-buffer-query-functions'.  (With the default value, this
will bypass confirmation before killing buffers with running
processes.)"
  (setq buffer-or-name (or buffer-or-name (current-buffer)))
  (when (buffer-live-p buffer-or-name)
    (with-current-buffer buffer-or-name
      (restore-buffer-modified-p nil)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-or-name)))))


;;; Extensions to `save-buffers-kill-terminal'
;;;===========================================
(defun aph/delete-frame-or-exit (&optional arg)
  "Delete this frame.  With only one frame, exit Emacs.

When there is more than one visible frame, run `delete-frame'.
Otherwise, exit Emacs with `save-buffers-kill-terminal' after
confirming this with user.

If a prefix ARG is supplied, ignore it in the multiple-frame
case.  Otherwise, bypass confirmation and pass the argument to
`save-buffers-kill-terminal'."
  (interactive "P")
  (cond
   ((> (length (visible-frame-list)) 1)  (delete-frame))
   ((or arg (y-or-n-p "Quit Emacs?"))    (save-buffers-kill-terminal arg))
   (t                                    (message "Abort"))))


;;; Make Emacs Source Read-Only
;;;============================
;; This code makes Emacs source code default to read-only mode.
;; Obtained from a stackexchange answer by user phils. 
(defvar aph/emacs-source-dirs
  '("/usr/local/src/emacs"
    "/usr/local/share/emacs"
    "/usr/share/emacs")
  "List of directories containing Emacs source.
Referenced by `aph/emacs-source-make-read-only'.")

(defun aph/package-install-writability (orig-fn &rest args)
  "Advice so `package-install' can write to ELPA directory.
Intended as :around advice for `package-install'."
  (dir-locals-set-directory-class package-user-dir 'default)
  (apply orig-fn args)
  (dir-locals-set-directory-class package-user-dir 'emacs))

(defun aph/emacs-source-make-read-only ()
  "Make Emacs source code open in read-only mode by default.
Affects only built-in Emacs files and ELPA packages, not init
files or other user code."
  (dir-locals-set-class-variables
   'default '((nil . ((buffer-read-only . nil))))) 
  (dir-locals-set-class-variables
   'emacs '((nil . ((buffer-read-only . t)))))

  ;; Built-in source
  (mapcar (lambda (dir)
            (dir-locals-set-directory-class dir 'emacs))
          aph/emacs-source-dirs)

  ;; Package code is a special problem, because these buffers need to be
  ;; writable for `package-install'.
  (dir-locals-set-directory-class package-user-dir 'emacs)
  (advice-add #'package-install :around #'aph/package-install-writability)) 


(provide 'aph-files)
