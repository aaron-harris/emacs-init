;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; DRAFT AREA
;;;;============================================================================

;; This file is for drafting new portions of the init files. It is not
;; automatically loaded on initialization.


;;;; Populate Region
;;==================
(require 'enumerate)
(require 's)

(defun populate-lines-in-region (beg end)
  "Copy non-blank lines downward in region.

Replace each blank line between BEG and END with the last
non-blank line appearing above it.  Leading blank lines are left
untouched.

Interactively, operate on the region if the region is active.  If
the region begins or ends mid-line, consider it to include that
entire line.  If there is no active region, operate on the entire
buffer.

A blank line for the purposes of this command is a line that
contains only whitespace characters.  It is not necessary that
the line be completely empty."
  (interactive "r")
  (let ((beg  (if (use-region-p) (enumerate--bol beg) (point-min)))
	(end  (if (use-region-p) (enumerate--eol end) (point-max)))
	stored-line)
    (save-excursion
      (save-restriction
	(goto-char beg)
	(narrow-to-region beg end)
	(while (progn
		 (beginning-of-line)
		 (if (looking-at "^\\s-*?$")
		     (when stored-line (replace-match stored-line))
		   (setq stored-line (s-chomp (thing-at-point 'line))))
		 (end-of-line)
		 (= (forward-line) 0)))))))

(bind-keys :map umbra-mode-map
	   ("s-<apps> p" . populate-lines-in-region))

(require 'aph-ert)

(ert-deftest draft-test-populate-lines-in-region ()
  "Test functionality of `populate-lines-in-region'."
  (aph/ert-with-buffer 'fundamental-mode "

Foo
\t
\s\s
Bar"
    (populate-lines-in-region (point-min) (point-max))
    (should (equal (buffer-string)
		   (concat "\n"
			   "Foo\n"
			   "Foo\n"
			   "Foo\n"
			   "Bar")))))

(provide 'init-draft)
