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
  (proctor-with-buffer 'fundamental-mode "

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


;;;; EIMP
;;=======
(defun aph/eimp-fit-image-to-window (&optional arg)
    "As `eimp-fit-image-to-window', but do not mark buffer as modified.
Also, ARG is optional, defaulting to nil."
    (let ((buffer     (current-buffer))
          (advice-id  (cl-gensym "aph/eimp-fit-image-to-window:")))
      ;; This is clearly a hack, and I should probably find a better
      ;; way to do this.
      (advice-add #'eimp-mogrify-process-sentinel :after
                  (lambda (proc msg)
                    (cond
                     ((not (buffer-live-p buffer))
                      (advice-remove #'eimp-mogrify-process-sentinel advice-id))
                     ((eq (process-buffer proc) buffer)
                      (with-current-buffer buffer
                        (set-buffer-modified-p nil)))))
                  `((name . ,advice-id)))
      ;; This wrapper fixes the situation where the current buffer is
      ;; not in the selected window, e.g., if we just opened the file
      ;; using `find-file-other-window'.
      (if (equal (selected-window) (get-buffer-window buffer))
          (eimp-fit-image-to-window arg) 
        (with-selected-window (next-window) ; Super-kludgy!
          (with-current-buffer buffer
            (eimp-fit-image-to-window arg))))))


;;;; Forms templates
;;==================
(defun forms-new-db-from-template (template dir name)
  "Make a new `forms-mode' database based on TEMPLATE.

Here, TEMPLATE is the path to an existing `forms-mode' control
file.  A new control file named NAME.ctrl is created in DIR, as
well as an empty database file NAME.db.  The resulting database
uses `load-file' to inherit all behavior except the value of
`forms-file' from TEMPLATE.

After the file is created, open it in `forms-mode'."
  (interactive "fCreate database from template: 
DCreate database in directory: 
sName for new database: ") 
  (let ((control-file  (expand-file-name (format "%s.ctrl" name) dir)))
    (with-temp-buffer
      (insert (concat ";; -*- mode: emacs-lisp -*-\n"
                      "\n"
                      (format "(load-file %S)\n" template)
                      (format "(setq forms-file \"%s.db\")\n" name)
                      "\n"
                      (format ";;; %s.ctrl ends here" name)))
      (write-region nil nil control-file nil nil nil :new))
    (forms-find-file control-file)))

(provide 'init-draft)
