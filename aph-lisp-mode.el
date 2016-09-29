;;; aph-lisp-mode.el --- Extensions for `lisp-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: tools, lisp

;; Dependencies: `lisp-mode'

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

;; Functions and commands extending those in the `lisp-mode' module
;; built into Emacs.

;;; Code:

(require 'lisp-mode)


;;;; Font Lock
;;============
(defun aph/emacs-lisp-add-font-lock-keywords ()
  "Add extra font lock keywords for Emacs lisp.
For use in `emacs-lisp-mode-hook', so it will apply to derived
modes, including `lisp-interaction-mode'."
  (font-lock-add-keywords
   nil
   '(;; Words inside `'; this overrides the default regexp, which does
     ;; not highlight single-character symbols such as `s'.
     ("`\\(\\(?:\\sw\\|\\s_\\)+\\)'"
      1 font-lock-constant-face t))))


;;;; Evaluation Commands
;;======================
(defun aph/eval-region-or-buffer ()
  "As `eval-region', or `eval-buffer' if region inactive."
  (interactive)
  (call-interactively
   (if (use-region-p) #'eval-region #'eval-buffer)))

(provide 'aph-lisp-mode)
;;; aph-lisp-mode.el ends here
