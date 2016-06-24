;;; aph-smartparens-test.el --- Tests for aph-smartparens.el -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>

;; Dependencies: `aph-smartparens', `proctor'

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

;;; Code:

(require 'aph-smartparens)
(require 'proctor)

(defun aph/sp-test-kill-sentence-1 (lines words chars arg start desired)
  "Subroutine for `aph/sp-test-kill-sentence'.

Move forward by LINES, WORDS, and CHARS; check that we're
`looking-at-p' START (a string); call `aph/sp-kill-sentence' with
ARG; and finally check that we're `looking-at-p' DESIRED."
  (declare (indent 4))
  (unless (zerop lines) (forward-line lines))
  (forward-word words)
  (forward-char chars)
  (should (looking-at-p start))
  (aph/sp-kill-sentence arg)
  (should (looking-at-p desired)))

(ert-deftest aph/sp-test-kill-sentence ()
  "Test `aph/sp-kill-sentence'."
  (proctor-with-buffer 'emacs-lisp-mode "
\(defun foo (arglist)
  \"This is a docstring.\"
  (do-stuff-here)
  ;; This is a comment
  (and \"foo\" \"bar\"))"
    (aph/sp-test-kill-sentence-1 1 1 0 -1
      " is a docstring."
      " is a docstring.")
    (aph/sp-test-kill-sentence-1 0 0 0 1
      " is a docstring."
      "\"") 
    (aph/sp-test-kill-sentence-1 2 1 0 1
      " is a comment"
      "\n  (and \"foo\"")
    (aph/sp-test-kill-sentence-1 1 1 2 1
      "foo\" \"bar\")" 
      "\" \"bar\")"))
  (proctor-with-buffer 'text-mode "
Sentence one.  Sentence two."
    (aph/sp-kill-sentence)
    (should (looking-at-p "  Sentence two."))))


(provide 'aph-smartparens-test)
;;; aph-smartparens-test.el ends here
