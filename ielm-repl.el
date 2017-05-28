;;; ielm-repl.el --- Use `ielm' as a repl            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: lisp

;; Dependencies: `ielm'
;; Functions advised from other packages:
;;   ielm: `ielm'

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

;; For many programming languages, the convention for using a REPL
;; from within Emacs is that the keybinding `C-c C-z' should switch to
;; the REPL from any buffer of that language, the REPL should be
;; displayed in the "other window" (usually on the right), and `C-c
;; C-z' from the REPL should return the user to the last buffer from
;; which the REPL was entered in this way.
;;
;; The Elisp REPL is `ielm', but `ielm' does not abide by these
;; conventions.  It doesn't display itself in the "other window", and
;; it doesn't provide any way to get back to the last buffer it was
;; invoked from.  This module adds this functionality.
;;
;; To switch to `ielm' as described above, use the command
;; `ielm-repl'.  To switch back, use `ielm-repl-switch-back'.  It is
;; recommended that you bind these command to `C-c C-z' in
;; `emacs-lisp-mode' and `inferior-emacs-lisp-mode', respectively, but
;; this module does not set up any keybindings.

;;; Code:

(require 'ielm)


;;;; State Variables
;;==================
(defvar ielm-repl--previous-buffer nil
  "The last buffer from which `ielm' was invoked.
Used by `ielm-repl-switch-back'.")


;;;; Commands
;;===========
;;;###autoload
(defun ielm-repl ()
  "As `ielm', but show ielm buffer in other window."
  (interactive)
  (setq ielm-repl--previous-buffer (current-buffer))
  (let ((display-buffer-alist
         (cons `(,(regexp-quote "*ielm*")
                 . (display-buffer-pop-up-window
                    . ((inhibit-same-window  . t)
                       (inhibit-switch-frame . t))))
               display-buffer-alist)))
    (ielm)))

(defun ielm-repl-switch-back ()
  "Switch to last buffer from which `ielm-repl' was invoked."
  (interactive)
  (if ielm-repl--previous-buffer
      (switch-to-buffer-other-window ielm-repl--previous-buffer)
    (error "Not sure which buffer to return to")))

(provide 'ielm-repl)
;;; ielm-repl.el ends here
