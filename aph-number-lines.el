;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; LINE NUMBERING FUNCTIONS
;;;;============================================================================

;; This module provides commands that number the lines in the region
;; in various ways, or modify line numbers already present.  These
;; functions are similar to, but not directly extensions of,
;; `rectangle-number-line'.


;;; Line Numbering
;;;===============
(defun aph/number-lines (beg end &optional offset eol sep format-str)
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

The FORMAT-STR argument can be used to configure how the
numbers are displayed.  This is a string similar to those used in
the `format' function, but all ordinary format specifiers should
be double-escaped (e.g., \"%%d\"), and the special escape \"%n\"
will be replaced with the maximum number of digits of any line
number before the other escapes are interpreted.  The default
value for FORMAT-STR is \"%%%nd\"."
  (interactive "r\np")
  (require 'aph-subr)               ; For `aph/get-bol', `aph/get-eol'
  (require 'dash)                   ; For `->>'
  (let* ((beg        (if (use-region-p) (aph/get-bol beg) (point-min)))
         (end        (if (use-region-p) (aph/get-eol end) (point-max)))
         (offset     (1- (or offset 1)))
         (sep        (or sep " "))
         (format-str (or format-str "%%%nd"))
         (regexp     (if eol "$" "^")) 
         (pad        (->> (count-lines beg end)
                          (max 1)
                          (+ offset 1)
                          (format "%d")
                          length))
         (formatter  (format-spec format-str (format-spec-make ?n pad))))
    (save-excursion
      (save-restriction
        (goto-char beg)
        (narrow-to-region beg end) 
        (while (progn
                 (if eol (end-of-line) (beginning-of-line))
                 (insert (concat (when eol sep)
                                 (format formatter
                                         (+ (line-number-at-pos) offset))
                                 (unless eol sep)))
                 (end-of-line)
                 (= (forward-line) 0)))))))

(defun aph/number-lines-alpha (beg end &optional offset sep format-string)
  "Number lines in region according to alphabetic order.

As `aph/number-lines', except lines are numbered according to
their alphabetic order instead of their position, and the option
to put the number at the end of the line is unavailable."
  (interactive "r\np")
  (require 'aph-subr)               ; For `aph/get-bol', `aph/get-eol'
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


;;; Working with Line Numbers
;;;==========================
;; Fnctions in this section are designed to manipulate existing line
;; numbers.

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
Here NUMS may be a comma- or whitespace-delimited string, or a list.

Interactively, prompt for NUMS."
  (interactive "r\nsNumbers to skip: ")
  (when (stringp nums)
    (setq nums
          (mapcar #'string-to-number
                  (split-string nums "[, \f\t\n\r\v]" :omit-nulls "\s-+"))))
  (mapcar (apply-partially #'aph/number-lines-open beg end) nums))


(provide 'aph-number-lines)
