;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; SMARTPARENS TESTS
;;;;============================================================================

;; Tests for the module aph-smartparens.el.
(require 'aph-smartparens)
(require 'aph-ert)


;;; String Protection
;;;==================
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
  (aph/ert-with-buffer 'emacs-lisp-mode "
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
  (aph/ert-with-buffer 'text-mode "
Sentence one.  Sentence two."
    (aph/sp-kill-sentence)
    (should (looking-at-p "  Sentence two."))))


(provide 'aph-smartparens-test)
