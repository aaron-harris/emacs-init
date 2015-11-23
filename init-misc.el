;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; MISCELLANEOUS SETTINGS
;;;;============================================================================

(require 'aph-require)                  ; For `aph/require-softly' 

;; Avy
(setq avy-style 'pre)
(setq avy-background t)
(setq avy-all-windows nil)
(setq avy-highlight-first t)

;; Calendar settings
(setq calendar-longitude -93.2
      calendar-latitude 45.0)

;; Clipboard settings (copied verbatim from better-defaults.el)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Completion settings
(setq completion-auto-help 'lazy)

;; Saved place and backup settings
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Scrolling settings
(setq scroll-margin 1 
      scroll-conservatively 1000
      scroll-preserve-screen-position :always)

;; Tooltip Settings
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Uniquify settings
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;;; Enabling Commands
;;;==================
;; These enable commands which are by default disabled.
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page  'disabled nil)
(put 'upcase-region   'disabled nil)


;;; Rectangle Patch
;;;================
;; There seems to be a bug in Emacs 25 where movement commands in
;; rectangle mode hang when given a prefix argument.  This is a
;; workaround until the issue is fixed.
(defun aph/rectangle-repetition-fix (fn cmd n &optional other-cmd)
  "Advice to fix bug in `rectangle-mark-mode' motion commands.

The basic cursor motion commands in `rectangle-mark-mode' that
were introduced in Emacs 25 do not currently handle their prefix
arguments correctly.  These commands (principally
`rectangle-forward-char' and `rectangle-backward-char') delegate
to `rectangle--*-char'.  This function fixes the problem when
used as advice :around `rectangle--*-char'."
  (cond
   ((< n 0)  (aph/rectangle-repetition-fix fn other-cmd (- n)))
   ((= n 0)  (funcall fn cmd 0 other-cmd))
   ((> n 0)  (dotimes (i (1- n) (funcall fn cmd 1 other-cmd))
               (funcall fn cmd 1 other-cmd)))))

;; Since the function `rectangle--*-char' is not defined in Emacs 24,
;; this next statement will fail silently if we don't need the fix.
(advice-add #'rectangle--*-char :around #'aph/rectangle-repetition-fix)

(provide 'init-misc)
