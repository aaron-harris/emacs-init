;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; WINDOW EXTENSIONS
;;;;============================================================================

;; Extensions to the `window' module.
(require 'bfw)
(require 'hydra)


;;; Extensions to `other-window'
;;;=============================
(defun aph/other-window-backward (count &optional all-frames)
  "As `other-window' but reversed."
  (interactive "p")
  (other-window (- count) all-frames))


;;; Sliding Buffer Subroutines
;;;===========================
(defun aph/slide-buffer (from-win to-win &optional select)
  "Move buffer from FROM-WIN to TO-WIN.

Display the buffer currenty displayed in FROM-WIN in TO-WIN.
Display in FROM-WIN the last buffer displayed there (as
`switch-to-prev-buffer').

If the optional parameter SELECT is supplied and non-nil, make
TO-WIN the selected window.

If either FROM-WIN or TO-WIN is nil, do nothing.  If both are the
same window, don't change any buffers but still select TO-WIN if
appropriate."
  (when (and from-win to-win)
    (let ((buf (window-buffer from-win)))
      (switch-to-prev-buffer from-win)
      (set-window-buffer to-win buf)
      (when select (select-window to-win)))))

(defun aph/swap-buffers (win1 win2)
  "Exchange buffers displayed in WIN1 and WIN2.
If either window is nil, do nothing."
  (when (and win1 win2)
    (let ((buf1 (window-buffer win1))
          (buf2 (window-buffer win2)))
      (set-window-buffer win1 buf2)
      (set-window-buffer win2 buf1))))


;;; Sliding Buffer Commands
;;;========================
(defun aph/pull-buffer-backward (&optional count)
  "Pull buffer from another window.

Display in this buffer the one currently displayed in the window
COUNT windows forward (in the same ordering as `other-window'),
skipping windows dedicated to their current buffers.  Display in
the window previously occupied by this buffer the previous buffer
displayed in that window (using `switch-to-prev-buffer')."
  (interactive "p")
  (aph/slide-buffer (bfw-get-nth-window-not-dedicated count)
                    (selected-window)))

(defun aph/pull-buffer-forward (&optional count)
  "As `aph/pull-buffer-backward' with direction reversed."
  (interactive "p")
  (aph/pull-buffer-backward (- count)))

(defun aph/slide-buffer-forward (&optional count ride)
  "Slide active buffer to another window.

Display this buffer COUNT windows forward (in the same ordering
as `other-window'), skipping windows dedicated to their current
buffers, and display in this window the previous buffer displayed
here (using `switch-to-prev-buffer').

If the optional parameter RIDE is supplied, \"ride\" the buffer,
making its new window the selected one.

As a special case, if COUNT is zero, treat COUNT as 1 and RIDE as
t.  This allows the RIDE parameter to be used interactively."
  (interactive "p")
  (if (zerop count) (aph/slide-buffer-forward 1 :ride)
    (aph/slide-buffer (selected-window)
                      (bfw-get-nth-window-not-dedicated count) ride)))

(defun aph/slide-buffer-backward (&optional count ride)
  "As `aph/slide-buffer-forward' with direction reversed."
  (interactive "p")
  (if (zerop count) (aph/slide-buffer-forward -1 :ride)
    (aph/slide-buffer-forward (- count) ride)))

(defun aph/swap-buffer-forward (&optional count ride)
  "Swap active buffer with that in another window.

Display this buffer COUNT forward (in the same ordering as
`other-window'), skipping windows dedicated to their current
buffers, and display in this window the buffer that was displayed
there.

If the optional parameter RIDE is supplied, \"ride\" the buffer,
making its new window the selected one.

As a special case, if COUNT is zero, treat COUNT as 1 and RIDE as
t.  This allows the RIDE parameter to be used interactively."
  (interactive "p")
  (if (zerop count) (aph/swap-buffer-forward 1 :ride)
    (let* ((win (bfw-get-nth-window-not-dedicated count)) 
           (buf (window-buffer win)))
      (aph/swap-buffers (selected-window) win)
      (when ride (select-window win)))))

(defun aph/swap-buffer-backward (&optional count ride)
  "As `aph/swap-buffer-forward' with direction reversed."
  (interactive "p")
  (if (zerop count) (aph/swap-buffer-forward -1 :ride)
    (aph/swap-buffer-forward (- count) ride)))

(defun aph/swap-buffer-forward-and-ride (&optional count)
  "As `aph/swap-buffer-forward' but always ride."
  (interactive "p")
  (aph/swap-buffer-forward count :ride))

(defun aph/swap-buffer-backward-and-ride (&optional count)
  "As `aph/swap-buffer-backward' but always ride."
  (interactive "p")
  (aph/swap-buffer-backward count :ride))


;;; Quit Help Windows
;;;==================

;; This variable is autoloaded to enable the easy addition of help
;; buffer types during initialization.

;;;###autoload
(defvar aph/help-window-names
  '(
    ;; Ubiquitous help buffers
    "*Help*"
    "*Apropos*"
    "*Messages*"
    "*Completions*"
    ;; Other general buffers
    "*Command History*"
    "*Compile-Log*"
    "*disabled command*")
  "Names of buffers that `aph/quit-help-windows' should quit.")

(defun aph/quit-help-windows (&optional kill frame)
  "Quit all windows with help-like buffers.

Call `quit-windows-on' for every buffer named in
`aph/help-windows-name'.  The optional parameters KILL and FRAME
are just as in `quit-windows-on', except FRAME defaults to t (so
that only windows on the selected frame are considered).

Note that a nil value for FRAME cannot be distinguished from an
omitted parameter and will be ignored; use some other value if
you want to quit windows on all frames."
  (interactive)
  (let ((frame (or frame t)))
    (dolist (buffer aph/help-window-names)
      (ignore-errors
        (quit-windows-on buffer kill frame)))))


;;; Scrolling Commands
;;;===================
(defhydra aph/hydra-scroll-other (:color pink)
  "Scroll other"
  ("C-v" scroll-other-window      "fwd")
  ("M-v" (scroll-other-window '-) "back") 
  ("C-g" nil                      "quit" :color blue))


(provide 'aph-window)
