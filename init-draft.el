;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; DRAFT AREA
;;;;============================================================================

;; This file is for drafting new portions of the init files. It is not
;; automatically loaded on initialization.


;;; `hippie-unexpand'
;;;==================
(define-key read-expression-map [(tab)] 'hippie-expand)

(defun hippie-unexpand ()
  (interactive)
  (hippie-expand -1))

(define-key read-expression-map [(shift tab)] 'hippie-unexpand)


;;; Improvements to C-h F
;;;======================
(defun aph/Info-find-manual-to-search (symbol &optional is-var)
  "Find the Info manual most likely to describe SYMBOL.

Here SYMBOL is the name of a function or variable.  If SYMBOL is
both the name of a function and the name of a variable, it is
interpreted as the function name unless the optional parameter
IS-VAR is non-nil."
  (let ((res (Info-find-emacs-command-nodes symbol)))
    (cond
     (res     (car res))
     (is-var  "emacs")
     (t       "elisp"))))

(defun aph/Info-goto-emacs-function-node (function)
  "Go to the Info node in the Emacs manual for FUNCTION.

Commands are passed directly to `Info-goto-emacs-command-node'.
For other functions, an index search is attempted."
  (interactive "aFind documentation for function: ")
  (let ((res nil))
    (cond
     ;; Send commands to `Info-goto-emacs-command-node'.
     ((commandp function)
      (Info-goto-emacs-command-node function))
     ;; Then see if `Info-find-emacs-command-nodes' finds anything.
     ;; This function is only documented to work for commands, but
     ;; seems to work fine for all functions and variables.
     ((Info-find-emacs-command-nodes function))
     ;; Finally, try an index search in an appropriate manual.
     (t
      (info (aph/Info-find-manual-to-search function))
      (Info-index (symbol-name function))))))

;; This will mostly follow `aph/Info-goto-emacs-function-node', once
;; that's working.
(defun aph/Info-goto-emacs-variable-node (variable)
  "Go to the Info node in the Emacs manual for VARIABLE."
  (interactive "vFind documentation for variable: ")
  (info (aph/Info-find-manual-to-search variable :var))
  (Info-index (symbol-name variable)))


;;; Adding font lock for `aph/defun-dyn'
;;;=====================================
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\(\\(aph/defun-dyn\\_>\\)" 1 font-lock-keyword-face)))


;;; Freeing `C-[' and `C-i'
;;;========================
(define-key input-decode-map [?\C-\[] (kbd "<C-[>"))


;;; Number Lines in Region
;;;=======================
(defun aph/get-bol (&optional pos)
  "Return position of first character on line containing POS.
If POS is omitted, use position of point.

Does not move point."
  (save-excursion
    (let ((pos (or pos (point))))
      (goto-char pos)
      (point-at-bol))))

(defun aph/get-eol (&optional pos)
  "Return position of last character on line containing POS.
If POS is omitted, use position of point.

Does not move point."
  (save-excursion
    (let ((pos (or pos (point))))
      (goto-char pos)
      (point-at-eol))))

(ert-deftest aph/test-get-bol/-eol ()
  "Test functions `aph/get-bol' and `aph/get-eol'."
  (with-temp-buffer
    (should (= (aph/get-bol) (aph/get-bol (point))
               (aph/get-eol) (aph/get-eol (point))
               (point) (point-min) (point-max)))
    (insert "A")
    (should (= (aph/get-bol) (aph/get-bol (point)) 1))
    (should (= (aph/get-eol) (aph/get-eol (point)) 2 (point)))
    (insert "B") 
    (should (= (aph/get-bol) (aph/get-bol (point)) 1))
    (should (= (aph/get-eol) (aph/get-eol (point)) 3 (point)))
    (insert "\n")
    (should (= (aph/get-bol 1) (aph/get-bol 3) 1))
    (should (= (aph/get-eol 1) (aph/get-eol 3) 3)) 
    (should (= (aph/get-bol) (aph/get-bol (point))
               (aph/get-eol) (aph/get-eol (point)) 4 (point)))
    (insert "foo\n\nbar")
    (should (= (aph/get-bol 4) (aph/get-bol 6) 4))
    (should (= (aph/get-eol 4) (aph/get-eol 6) 7))
    (should (= (aph/get-bol 8) (aph/get-eol 8) 8))
    (should (= (aph/get-bol 9) (aph/get-bol) 9))
    (should (= (aph/get-eol 9) (aph/get-eol) 12 (point)))))

(defun aph/number-lines (beg end &optional offset eol sep format-string)
  "Insert a number for each line in the region.
Non-interactively, act on the text between BEG and END.

If the region is not active, act on the entire buffer.

If the region to be acted on starts or ends in the middle of a
line, a line number will still be inserted for that line.

If OFFSET is supplied (interactively, as a numeric prefix
argument), it is used as a starting point for the numbering;
otherwise, it defaults to 1.

If EOL is non-nil, the numbers will be appended to the end of
each line instead of being prepended to the beginning.

The SEP argument is the string separating the line number from
the rest of the line; i.e., it will ordinarily follow the number
but precedes it if EOL is supplied.  This defaults to a single
space.

The FORMAT-STRING argument can be used to configure how the
numbers are displayed.  This is a string similar to those used in
the `format' function, but all ordinary format specifiers should
be double-escaped (e.g., \"%%d\"), and the special escape \"%n\"
will be replaced with the maximum number of digits of any line
number before the other escapes are interpreted.  The default
value for FORMAT-STRING is \"%%%nd\"."
  (interactive "r\np")
  (require 'dash)                       ; For `->>'
  (let* ((beg           (if (use-region-p) (aph/get-bol beg) (point-min)))
         (end           (if (use-region-p) (aph/get-eol end) (point-max)))
         (offset        (1- (or offset 1)))
         (sep           (or sep " "))
         (format-string (or format-string "%%%nd"))
         (regexp        (if eol "$" "^")) 
         (pad           (->> (count-lines beg end)
                             (max 1)
                             (+ offset)
                             (format "%d")
                             length))
         (formatter     (format-spec format-string (format-spec-make ?n pad))))
    (save-excursion
      (save-restriction
        (goto-char beg)
        (narrow-to-region beg end)
        (while (progn
                 (if eol (end-of-line) (beginning-of-line))
                 (insert (concat (when eol sep)
                                 (format formatter (+ (line-number-at-pos) offset))
                                 (unless eol sep)))
                 (end-of-line)
                 (= (forward-line) 0)))))))

(defun aph/number-lines-alpha (beg end &optional offset sep format-string)
  "Number lines in region according to alphabetic order.

As `aph/number-lines', except lines are numbered according to
their alphabetic order instead of their position, and the option
to put the number at the end of the line is unavailable."
  (interactive "r\np")
  (let* ((beg (if (use-region-p) (aph/get-bol beg) (point-min)))
         (end (if (use-region-p) (aph/get-eol end) (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        ;; Append ordinary line numbers to eol, sort, then add the
        ;; numbers we want to bol.
        (aph/number-lines (point-min) (point-max) 1 :eol " " "%%0%nd")
        (sort-lines nil (point-min) (point-max))
        (aph/number-lines (point-min) (point-max) offset nil sep format-string)
        ;; Move the eol line numbers to bol and sort again to recover
        ;; original order.
        (goto-char (point-min))
        (while (re-search-forward "^\\(.*\\) \\([0-9]+\\)$" nil :noerr)
          (replace-match "\\2 \\1"))
        (sort-lines nil (point-min) (point-max))
        ;; Remove extraneous numbers.
        (goto-char (point-min))
        (while (re-search-forward "^\\([0-9]+\\) " nil :noerr)
          (replace-match "")
          (end-of-line))))))

(defun aph/number-lines-open (beg end n)
  "Renumber lines in region to skip N.
Non-interactively, act on the text between BEG and END.

Interpreting any sequence of digits at the beginning of a line as
a line number, increment all line numbers in the region that are
at least N by one.

Attempt to match line number formatting (e.g., leading zeros)
when this is not too difficult.

If the region is not active, act on the entire buffer."
  (interactive "r\nNNumber to skip: ")
  (let ((beg (if (use-region-p) beg (point-min)))
        (end (if (use-region-p) end (point-max))))
    (goto-char beg)
    (while (re-search-forward "^[0-9]+" end :noerr)
      (let* ((str       (match-string 0))
             (k         (string-to-number str))
             (len       (length str))
             (zeros     (equal (substring str 0 1) "0"))
             (formatter (format "%%%s%dd" (if zeros "0" "") len)))
        (when (>= k n)
          (replace-match (format formatter (1+ k))))))))

(defun aph/number-lines-open-multiple (beg end nums)
  "Renumber lines in region to skip all NUMS.
Here NUMS may be a comma- or whitespace-delimited string, or a list."
  (interactive "r\nsNumbers to skip: ")
  (when (stringp nums)
    (setq nums
          (mapcar #'string-to-number
                  (split-string nums "[, \f\t\n\r\v]" :omit-nulls "\s-+"))))
  (mapcar (apply-partially #'aph/number-lines-open beg end) nums))

(aph/global-set-keys-safely
  ((kbd "s-<apps> n")       #'aph/number-lines)
  ((kbd "s-<apps> C-n")     #'aph/number-lines-alpha)
  ((kbd "s-<apps> M-n o")   #'aph/number-lines-open)
  ((kbd "s-<apps> M-n M-o") #'aph/number-lines-open-multiple))

(ert-deftest aph/test-number-lines ()
  "Test basic functionality of `aph/number-lines'."
  (with-temp-buffer
    (aph/number-lines (point-min) (point-max))
    (should (equal (buffer-string) "1 ")))
  (with-temp-buffer
    (insert "A\nB\nC")
    (aph/number-lines (point-min) (point-max))
    (should (equal (buffer-string) "1 A\n2 B\n3 C")))
  (with-temp-buffer
    (insert (make-string 99 ?\n))
    (aph/number-lines (point-min) (point-max))
    (should (equal (buffer-string) 
                   (mapconcat (apply-partially #'format "%3d ")
                              (number-sequence 1 100)
                              "\n")))))


;;; Mode Tags
;;;==========
(defmacro aph/mode-tag-create (tag &optional docstring)
  "Define TAG as a mode tag.

A mode tag is a way to designate several otherwise-unrelated
modes (major or minor) as sharing some particular
characteristic.

To tag a mode with a mode tag, use `aph/mode-tag-add'.  To remove
a tag, use `aph/mode-tag-remove'.  To check for the existence of
a tag, use `aph/mode-tag-p'.  To check for the association
between modes and tags, use `aph/mode-tag-p',
`aph/mode-tag-get-tags-for-mode', or
`aph/mode-tag-get-modes-for-tag'.  To delete an existing mode
tag, use `aph/mode-tag-delete'.

Defining a mode tag creates a hook variable named `TAG-tag-hook',
and all modes tagged with a tag (or derived from such a mode) run
this hook along with their individual mode hooks.  If a variable
with this name already exists, an error is signalled and the tag
is not created.

Information about mode tags is primarily stored using symbol
properties.  Four properties are used; except for `aph/mode-tag-tags',
these properties are associated with the symbol naming the mode tag.
- `aph/mode-tag' is set to t for symbols which name mode tags.
- `aph/mode-tag-modes' stores a list of all modes tagged with a
  particular tag.
- `aph/mode-tag-tags' (on the *mode* name) stores a list of all tags
  associated with that mode.
- `aph/mode-tag-docstring' stores the docstring for the tag.  The
  functionality to reference this (e.g., in a Help buffer) has not yet
  been implemented."
  (declare (debug (&define name [&optional stringp]))) 
  (let ((hook (intern (concat (symbol-name tag) "-tag-hook")))) 
    `(progn
      (when (boundp ',hook)
        (error "Variable %s already exists; tag %s not created" ',hook ',tag))
      (put ',tag 'aph/mode-tag t)
      (put ',tag 'aph/mode-tag-docstring ,docstring)
      (defvar ,hook nil
        ,(format "Hook run for all modes tagged with the mode tag `%s'.
No problems result if this variable is not bound.
`add-hook' automatically binds it.  (This is true for all hook variables.)"
                tag)))))

(ert-deftest aph/mode-tag-test-create--basic ()
  "Test basic functionality of `aph/mode-tag-create'."
  (require 'cl-lib)
  (let* ((tag        (cl-gensym "tag"))
         (docstring  "doc")
         (hook       (intern (concat (symbol-name tag) "-tag-hook"))))
    (unwind-protect
        (progn (eval `(aph/mode-tag-create ,tag ,docstring))
               (should (get tag 'aph/mode-tag))
               (should (equal docstring (get tag 'aph/mode-tag-docstring)))
               (should (boundp hook))
               (should (null (eval hook))))
      (setplist tag nil)
      (makunbound hook)
      (put hook 'variable-documentation nil))))

(defun aph/cleanup (sym)
  (makunbound sym)
  (put sym 'variable-documentation nil))

(defun aph/mode-tag-delete (tag)
  "Delete TAG as a mode tag.
See `aph/mode-tag-create' for more information on mode tags.")

(defun aph/mode-tag-add (mode tag)
  "Tag MODE with TAG.
See `aph/mode-tag-create' for more information on mode tags.")

(defun aph/mode-tag-remove (mode tag &optional nowarn)
  "If MODE is tagged with TAG, remove it.
If MODE is not tagged with TAG, print a warning message unless
the optional argument NOWARN is non-nil.

See `aph/mode-tag-create' for more information on mode tags.")

(defun aph/mode-tag-p (sym)
  "Return non-nil if SYM is the name of a mode tag.
See `aph/mode-tag-create' for more information on mode tags.")

(defun aph/mode-tag-tagged-p (mode tag &optional inherit)
  "Return non-nil if MODE is tagged with TAG.
If the optional parameter INHERIT is non-nil, also return non-nil
if any ancestor of MODE is tagged with TAG.

See `aph/mode-tag-create' for more information on mode tags.")

(defun aph/mode-tag-get-tags-for-mode (mode)
  "Return a list of all mode tags on MODE.
See `aph/mode-tag-create' for more information on mode tags.")

(defun aph/mode-tag-get-modes-for-tag (tag)
  "Return a list of all modes tagged with TAG.
See `aph/mode-tag-create' for more information on mode tags.")
