;;; forms-narrow.el --- Narrowing for `forms-mode'   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: data, forms

;; Dependencies `forms-barb', `seq'

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

;; This module implements a form of narrowing for `forms-mode'
;; databases.  When narrowing is in effect, some records are "hidden",
;; so that navigation commands will skip over them.  Note that this
;; functionality is distinct from the built-in narrowing functionality
;; (e.g., `narrow-to-region'), which cannot affect more than a single
;; record in `forms-mode'.
;;
;; To use this module, just open a `forms-mode' database and invoke a
;; narrowing command.  At present, there is only one narrowing
;; command, but more are planned:
;;
;; `forms-narrow-regexp':
;;
;;     Show only records matching the specified regexp (in any field).
;;
;; To remove the narrowing effect, use the command
;; `forms-narrow-widen'.  There is also the command
;; `forms-narrow-again', which applies whatever narrowing was last
;; used this session.
;;
;; When the database is narrowed, by default the current record is
;; changed to the first visible record in the database.  To change
;; this behavior, set the option `forms-narrow-rebase-mode'.  See its
;; docstrings for all the options.
;;
;; None of these commands have default keybindings, but they are
;; collected in the map `forms-narrow-map' (not
;; `forms-narrow-mode-map'; see below) for use as a prefix map.
;; Moreoever, if you wish to use this map instead of the default
;; narrowing map at `C-x n' in `forms-mode' buffers, just add the
;; function `forms-narrow-shadow' to `forms-mode-hook'.
;;
;; Custom narrowing commands are also supported.  Just pass a function
;; of no arguments to the function `forms-narrow'.  A record will only
;; be shown if your function returns non-nil when that record is
;; current.  Alternatively, you can pass a list of record numbers to
;; `forms-narrow-list' for the same effect.
;;
;; Replacement of the basic `forms-mode' navigation commands (e.g.,
;; `forms-next-record') with narrowing-aware versions is implemented
;; via the minor mode `forms-narrow-mode', but in general you should
;; not need to enable this mode directly, as the narrowing and
;; widening commands will turn it on and off as necessary.
;;
;; While narrowed, the following commands are replaced with their
;; given narrowing-aware counterparts:
;;
;;   `forms-next-record'  => `forms-narrow-next-record'
;;   `forms-prev-record'  => `forms-narrow-prev-record'
;;   `forms-first-record' => `forms-narrow-first-record'
;;   `forms-last-record'  => `forms-narrow-last-record'
;;
;; Other commands (e.g., `forms-jump-record' or
;; `forms-search-forward') can land on a record not satisfying the
;; current narrowing criteria, but will not cancel the narrowing.

;;; Code:

(require 'forms-barb) 
(require 'seq)


;;;; User Options
;;===============
(defcustom forms-narrow-rebase-mode :first
  "Determines when current record should change when narrowing.

When a `forms-mode' database is narrowed with `forms-narrow', it
may also change what record is current, according to the value of
this option.  Possible values are as follows:

:first

    Always go to the first record that satisfies the new
    predicate.  This is the default behavior.

:next

    Stay on the current record if it satisfies the new predicate;
    otherwise, go to the next record that does satisfy the
    predicate.  If all following records do not satisfy the
    predicate, wrap and go the first.

Any other value

    Do not change the current record, even if the current record does
    not satisfy the new predicate."
  :group 'forms
  :type '(choice (const :tag "First record" :first)
                 (const :tag "Next record"  :next)
                 (other :tag "Don't move"   nil)))

(defcustom forms-narrow-rebase-widen nil
  "Whether to use `forms-narrow-rebase-mode' when widening.

By default the option `forms-narrow-rebase-mode' only applies
when narrowing a `forms-mode' database.  If this option is
non-nil, it also applies when widening.

Note that this only matters if `forms-narrow-rebase-mode' is set
to :first, since the current record will always satisfy the
predicate after widening.")


;;;; State Variables
;;==================
(defvar-local forms-narrow--predicate nil
  "Predicate determining which records to show.

This variable may contain either a function or a list of
integers, which must be sorted in increasing order.

A function is called with no arguments whenever a new record is
displayed.  If it returns nil, that record is skipped.

A list of integers is interpreted as an implicit predicate.  Only
records whose record numbers appear in the list will be
displayed.  Representing the predicate in this form allows for
more efficient navigation.

This variable is not consulted unless `forms-narrow-mode' is
active.")


;;;; Basic Subroutines
;;====================
(defun forms-narrow-visible-p ()
  "Non-nil if current record satisfies current narrowing."
  (or (not forms-narrow-mode)
      (cond
       ((functionp forms-narrow--predicate)
        (funcall forms-narrow--predicate))

       ((listp forms-narrow--predicate)
        (member forms--current-record forms-narrow--predicate)))))


;;;; Navigation Commands
;;======================
(defun forms-narrow-next-record--list (arg)
  "Subroutine of `forms-narrow-next-record' for list narrowing."
  (let ((next-rec
         (->> forms-narrow--predicate
              (seq-drop-while (apply-partially #'>= forms--current-record))
              (nth (1- arg)))))
    (if next-rec
        (forms-jump-record next-rec)
      (error "Last visible record"))))

(defun forms-narrow-next-record--function (arg)
  "Subroutine of `forms-narrow-next-record' for function narrowing."
  (let ((stepper    (if (< arg 0) #'forms-prev-record #'forms-next-record))
        (saved-rec  forms--current-record))
    (forms-barb-with-single-record-change
     (condition-case err 
         (dotimes (i (abs arg))
           (while (progn (funcall stepper 1)
                         (not (forms-narrow-visible-p))))
           (setq saved-rec forms--current-record))
       (error (forms-jump-record saved-rec)
              (signal (car err) (cdr err)))))))

(defun forms-narrow-next-record (arg)
  "As `forms-next-record', but skip hidden records.

If all records after the current one are not visible, then signal
an error and stay on this record.

Run `forms-barb-change-record-hook' only once, on the final
record."
  (interactive "p")
  (funcall
   (cond ((not forms-narrow-mode)
          #'forms-next-record)
         ((functionp forms-narrow--predicate)
          #'forms-narrow-next-record--function)
         ((listp forms-narrow--predicate)
          #'forms-narrow-next-record--list))
   arg))

(defun forms-narrow-prev-record (arg)
  "As `forms-prev-record', but skip hidden records.

If all records before the current one are not visible, then
signal an error and stay on this record."
  (interactive "p")
  (forms-narrow-next-record (- arg)))

(defun forms-narrow-first-record ()
  "As `forms-first-record', but skip hidden records.

If all records are hidden, then signal an error and stay on this
record."
  (interactive)
  (forms-barb-with-single-record-change
   (forms-first-record)
   (unless (forms-narrow-visible-p)
     (forms-narrow-next-record 1))))

(defun forms-narrow-last-record ()
  "As `forms-last-record', but skip hidden records.

If all records are hidden, then signal an error and stay on this
record."
  (interactive)
  (forms-barb-with-single-record-change
   (forms-last-record)
   (unless (forms-narrow-visible-p)
     (forms-narrow-prev-record 1))))


;;;; Minor Mode and Keymap Setup
;;==============================
(defvar forms-narrow-mode-map
  (let ((keymap (make-sparse-keymap)))
    (dolist (pair
             '((forms-next-record  . forms-narrow-next-record)
               (forms-prev-record  . forms-narrow-prev-record)
               (forms-first-record . forms-narrow-first-record)
               (forms-last-record  . forms-narrow-last-record)))
      (define-key keymap `[remap ,(car pair)] (cdr pair)))
    keymap)
  "Keymap for `forms-narrow-mode'.")

(defun forms-narrow--rebase ()
  "Select an appropriate record after entering `forms-narrow-mode'.
Obey the user option `forms-narrow-rebase-mode'."
  (cond
   ((eq forms-narrow-rebase-mode :first)
    (forms-narrow-first-record))

   ((eq forms-narrow-rebase-mode :next)
    (unless (forms-narrow-visible-p)
      (ignore-errors (forms-narrow-next-record 1))
      (unless (forms-narrow-visible-p)
        (forms-narrow-first-record))))))

(define-minor-mode forms-narrow-mode
  "Minor mode for narrowed `forms-mode' databases."
  :group forms
  :lighter " Narrow"
  :keymap  'forms-narrow-mode-map
  :require 'forms-narrow
  (when (or forms-narrow-mode forms-narrow-rebase-widen)
    (forms-narrow--rebase)))

(defvar forms-narrow-map
  (let ((keymap (make-sparse-keymap)))
    (dolist (pair
             '(("w" . forms-narrow-widen)
               ("r" . forms-narrow-regexp)
               ("a" . forms-narrow-again)))
      (define-key keymap (kbd (car pair)) (cdr pair)))
    keymap)
  "Prefix map for narrowing commands in `forms-mode'.

By default, this prefix map is not bound to any key.  If you wish
to replace the default narrowing map (e.g., at `C-x n'), see the
function `forms-narrow-shadow'.")

;;;###autoload
(defun forms-narrow-shadow ()
  "Bind `forms-narrow-map' to `C-x n' in this buffer.

This binding will shadow entirely other, lower-precedence
keymaps (rather than merging their bindings).

If you want this to be the default in `forms-mode', add this
function to `forms-mode-hook'."
  (define-key forms-narrow-map (kbd "C-h") nil)
  (define-key forms-narrow-map [t] #'undefined)
  (local-set-key (kbd "C-x n") forms-narrow-map))


;;;; Entry and Exit Points
;;========================
(defun forms-narrow (pred)
  "Narrow the database to show only records satisfying PRED.
For use in `forms-mode'.

PRED is passed to `forms-narrow--predicate' directly, so it may
be either a function or a list.  List arguments will be
sorted."
  (unless (functionp pred)
    (setq pred (seq-sort #'<= pred)))
  (setq forms-narrow--predicate pred)
  (forms-narrow-mode 1))

(defun forms-narrow-widen (&optional verbose)
  "Remove narrowing for current database."
  (interactive "p")
  (when verbose (message "Widening database"))
  (forms-narrow-mode -1))

;;;###autoload
(defun forms-narrow-again (&optional verbose)
  "Narrow current database using last narrowing critera."
  (interactive "p")
  (when verbose (message "Narrowing database using last-used criteria"))
  (if (null forms-narrow--predicate)
      (error "No previous narrowing to re-use")
    (forms-narrow-mode 1)))


;;;; Narrowing Commands and Subroutines
;;=====================================
;;;###autoload
(defun forms-narrow-regexp (regexp)
  "Narrow to records matching REGEXP in any field."
  (interactive "sNarrow with regexp: ")
  (forms-narrow
   (lambda ()
     (seq-some (apply-partially #'string-match-p regexp)
               (cdr forms-fields)))))

(provide 'forms-narrow)
;;; forms-narrow.el ends here
