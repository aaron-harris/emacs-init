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


;;;; Indentation
;;==============

;; Taken from
;;     https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
;; via
;;     http://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
(defun aph/lisp-indent-function (indent-point state)
  "As `lisp-indent-function', but properly indent keyword lists.

For example, `lisp-indent-function' produces the indentation

    (:foo frobnicate
          :bar ((key1 . val1)
                (key2 . val2)))

whereas this function does this instead:

    (:foo frobnicate
     :bar ((key1 . val1)
           (key2 . val2)))"
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))


;;;; Evaluation Commands
;;======================
(defun aph/eval-region-or-buffer ()
  "As `eval-region', or `eval-buffer' if region inactive."
  (interactive)
  (call-interactively
   (if (use-region-p) #'eval-region #'eval-buffer)))

(provide 'aph-lisp-mode)
;;; aph-lisp-mode.el ends here
