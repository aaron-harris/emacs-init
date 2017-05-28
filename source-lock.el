;;; source-lock.el --- Make source files read-only   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: files

;; Dependencies: `seq', `validate' (optional)
;; Advised functions from other packages:
;;   package: `package-install'

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

;; This module defines the global minor mode `source-lock-mode', which
;; causes all Emacs source files to open in read-only mode.
;;
;; To use this module, just enable `source-lock-mode', e.g. by putting
;;
;;     (source-lock-mode t)
;;
;; in your personal configuration or by manually executing
;;
;;     M-x source-lock-mode
;;
;; By default, `source-lock-mode' will attempt to protect the Elisp
;; sources distributed with Emacs, in these directories
;;
;;     /usr/local/src/emacs
;;     /usr/local/share/emacs
;;     /usr/share/emacs
;;
;;
;; This list is configurable as the variable
;; `source-lock-directories'.  It isn't even limited to elisp files,
;; so you can add directories unrelated to Emacs if you wish.
;;
;; `source-lock-mode' also attempts to protect the source of ELPA
;; packages installed in `package-user-dir'; this is somewhat more
;; complicated, because provision needs to be made to install new
;; packages.  To disable this protection, set the variable
;; `source-lock-protect-packages-p' to nil.
;;
;; Inspiration for this module was provided by this StackExchange
;; answer by the user phils:
;;     http://emacs.stackexchange.com/a/3681/8647

;;; Code:

(require 'seq)


;;;; User Options
;;;;=============
(defgroup source-lock nil
  "Make source files read-only."
  :prefix "source-lock-"
  :link '(emacs-commentary-link "source-lock")
  :group 'files)

(defcustom source-lock-directories
  '("/usr/local/src/emacs"
    "/usr/local/share/emacs"
    "/usr/share/emacs")
  "List of directories protected by `source-lock-mode'.

If you wish to protect the source of ELPA packages, you can't
just put `package-user-dir' in this list, because this will
interfere with the ability to install packages.  Instead, set
`source-lock-protect-packages-p' to a non-nil value.

If you change this variable after the `source-lock' module has
been loaded, you may need to call `source-lock-refresh' before
the change is effective."
  :type '(repeat string))

(defcustom source-lock-protect-packages-p t
  "If non-nil, `source-lock-mode' will protect ELPA packages.

For the most part, this is the same as having `package-user-dir'
in `source-lock-directories', except provision is made for
installing new packages.

If you change this variable after the `source-lock' module has
been loaded, you may need to call `source-lock-refresh' before
the change is effective."
  :type 'boolean)


;;;; Directory Management
;;;;=====================
(defun source-lock--apply ()
  "Apply `source-lock-mode' protection to chosen directories.

The directories affected are those listed in
`source-lock-directories', plus `package-user-dir' if
`source-lock-protect-packages-p' is non-nil.

This function just tells `source-lock-mode' which directories to
protect (using directory-local variable classes); it does nothing
unless `source-lock-mode' is enabled."
  ;; Validate variables
  (when (require 'validate nil :noerror)
    (validate-variable 'source-lock-directories)
    (when source-lock-protect-packages-p
      (validate-variable 'package-user-dir)))
  ;; Apply protection
  (dolist (dir source-lock-directories)
    (dir-locals-set-directory-class dir 'source-lock))
  (when source-lock-protect-packages-p
    ;; If `package-user-dir' is symlinked, we need to protect both the
    ;; raw directory and its true location, because different commands
    ;; may use different paths.
    (dolist (dir `(,package-user-dir
                   ,(file-truename package-user-dir)))
      (dir-locals-set-directory-class dir 'source-lock))))

(defun source-lock--strip ()
  "Remove `source-lock-mode' protection from all directories."
  (setq dir-locals-directory-cache
        (seq-remove (lambda (entry) (eq (cadr entry) 'source-lock))
                    dir-locals-directory-cache)))

(defun source-lock-refresh ()
  "Refresh `source-lock-mode' protection.

Calling this function ensures that the directories
`source-lock-mode' is protecting are exactly those specified by
the variables `source-lock-directories' and
`source-lock-protect-packages-p'."
  (source-lock--strip)
  (source-lock--apply))


;;;; Protection Toggling
;;;;====================
(defun source-lock--protect (enable)
  "Enable or disable `source-lock-mode' protection.

If ENABLE is non-nil, enable `source-lock-mode' protection.
Otherwise, disable it.

This is used internally by `source-lock-mode'; for most purposes,
you should probably use that mode directly."
  (let ((vars-alist (when enable '((buffer-read-only . t)))))
    (dir-locals-set-class-variables
     'source-lock `((nil . ,vars-alist)))))

(defun source-lock-bypass (fn &rest args)
  "Call FN with ARGS, bypassing `source-lock-mode' protection."
  (if (not source-lock-mode)
      (apply fn args)
    (source-lock--protect nil)
    (unwind-protect (apply fn args)
      (source-lock--protect t))))

;;;###autoload
(define-minor-mode source-lock-mode
  "Mode to make source files read-only.

When enabled, any file in one of the directories listed in
`source-lock-directories' will be opened in read-only mode.  If
`source-lock-protect-packages-p' is non-nil, then
`package-user-dir' is similarly protected."
  :global t
  ;; Enable/disable protection
  (source-lock--protect source-lock-mode)
  ;; Allow for package installation
  (if (and source-lock-mode source-lock-protect-packages-p)
      (advice-add 'package-install :around #'source-lock-bypass)
    (advice-remove 'package-install #'source-lock-bypass)))


;;;; Initialization
;;;;===============
(dir-locals-set-class-variables 'source-lock '((nil)))
(source-lock--apply)


;;;; Unloading
;;;;==========
(defun source-lock-unload-function ()
  "Undo changes made to Emacs for `source-lock-mode'.

Specifically,

 * Ensure that no directory is assigned the `source-lock'
   directory class.

 * Delete the `source-lock' directory class entirely.

 * Remove advice supporting `source-lock-mode' from the function
   `package-install'."
  (source-lock-mode nil)                ; Advice removed here.
  (source-lock--strip)
  (setq dir-locals-class-alist
        (assq-delete-all 'source-lock dir-locals-class-alist)))

(provide 'source-lock)
;;; source-lock.el ends here
