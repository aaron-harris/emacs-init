;;; deck.el --- Deck-based buffer control            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: convenience

;; Dependencies: `bfw'

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

;; This module defines several commands that allow for individual
;; control over buffer placement (i.e., which buffer each window is
;; displaying).
;;
;; The inspiration for this module comes in viewing each window as
;; holding a stack of buffers, like a deck of cards, with only the top
;; buffer being displayed.  The existing commands `previous-buffer'
;; and `next-buffer' rearrange the deck for a particular window, and
;; these commands "move" buffers from one deck to another.
;;
;; Each command acts on the current buffer and accepts a numerical
;; argument for selecting the target window; this behaves in the same
;; way as `other-window', except it will skip windows dedicated to
;; their buffers.  All commands come in forward and backward versions.
;;
;; In the following list of commands, the description of each command
;; in terms of the "deck analogy" is followed by an example in
;; symbolic notation.  In this notation, buffers are represented by
;; letters and windows as bracketed groups of letters (buffers).  The
;; first buffer in the group is the buffer currently displayed by that
;; window, the second buffer is the buffer that would be shown
;; following an invocation of `previous-buffer', and so forth.  The
;; selected window is represented by double brackets, and any window
;; that is not initially selected is the target of the command.
;;
;; For instance, this is the way `other-window' would be represented
;; in this notation:
;;
;;   [[A]] [X] => [A] [[X]]
;;
;; And here is `previous-buffer':
;;
;;   [[AB]] => [[B]]

;;;; Command descriptions:
;;
;; `deck-pull-buffer-backward', `deck-pull-buffer-forward':
;;
;;     Pull the buffer from the top of the target window's deck to the
;;     top of the current window's deck.
;;       [[A]] [XY] => [[XA]] [Y]
;;
;; `deck-push-buffer-backward', `deck-push-buffer-forward':
;;
;;     Push the buffer from the top of the current window's deck to
;;     the top of the target window's deck.
;;       [[AB]] [X] => [[B]] [AX]
;;
;; `deck-surf-buffer-backward', `deck-surf-buffer-forward':
;;
;;     As "push", but also select the target window.
;;       [[AB]] [X] => [B] [[AX]]
;;
;; `deck-swap-buffer-backward', `deck-swap-buffer-forward':
;;
;;     Exchange the top buffer of the current window's deck with the
;;     top buffer of the target window's deck.
;;       [[AB]] [XY] => [[XB]] [AY]
;;
;; `deck-surf-swap-buffer-backward', `deck-surf-swap-buffer-forward':
;;
;;     As "swap", but also select the target window.
;;       [[AB]] [XY] => [XB] [[AY]]

;;; Code:

(require 'bfw)


;;;; Subroutines
;;=============
(defun deck--slide-buffer (from-win to-win &optional select)
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

(defun deck--swap-buffers (win1 win2)
  "Exchange buffers displayed in WIN1 and WIN2.
If either window is nil, do nothing."
  (when (and win1 win2)
    (let ((buf1 (window-buffer win1))
          (buf2 (window-buffer win2)))
      (set-window-buffer win1 buf2)
      (set-window-buffer win2 buf1))))


;;;; Commands
;;===========
;;;###autoload
(defun deck-pull-buffer-backward (&optional count)
  "Pull buffer from another window.

Display in this buffer the one currently displayed in the window
COUNT windows forward (in the same ordering as `other-window'),
skipping windows dedicated to their current buffers.  Display in
the window previously occupied by this buffer the previous buffer
displayed in that window (using `switch-to-prev-buffer')."
  (interactive "p")
  (deck--slide-buffer (bfw-get-nth-window-not-dedicated count)
                      (selected-window)))

;;;###autoload
(defun deck-pull-buffer-forward (&optional count)
  "As `deck-pull-buffer-backward' with direction reversed."
  (interactive "p")
  (deck-pull-buffer-backward (- count)))

;;;###autoload
(defun deck-push-buffer-forward (&optional count)
  "Slide active buffer to another window.

Display this buffer COUNT windows forward (in the same ordering
as `other-window'), skipping windows dedicated to their current
buffers, and display in the current window the previous buffer
displayed here (using `switch-to-prev-buffer').

As a special case, if COUNT is zero, surf forward one window (see
`deck-surf-buffer-forward')."
  (interactive "p")
  (if (zerop count) (deck-surf-buffer-forward 1)
    (deck--slide-buffer (selected-window)
                        (bfw-get-nth-window-not-dedicated count))))

;;;###autoload
(defun deck-push-buffer-backward (&optional count)
  "As `deck-push-buffer-forward' with direction reversed."
  (interactive "p")
  (if (zerop count) (deck-push-buffer-forward -1)
    (deck-push-buffer-forward (- count) ride)))

;;;###autoload
(defun deck-surf-buffer-forward (&optional count)
  "As `deck-push-buffer-forward', but select target window.

As a special case, if COUNT is zero, act as if COUNT were 1 but
do not select the target window.  This is for convenience in
interactive usage."
  (interactive "p")
  (if (zerop count) (deck-push-buffer-forward 1)
    (deck--slide-buffer (selected-window)
                        (bfw-get-nth-window-not-dedicated count)
                        :select)))

;;;###autoload
(defun deck-surf-buffer-backward (&optional count)
  "As `deck-push-buffer-backward', but select target window.

As a special case, if COUNT is zero, act as if COUNT were 1 but
do not select the target window.  This is for convenience in
interactive usage."
  (interactive "p")
  (if (zerop count) (deck-push-buffer-forward -1)
    (deck-surf-buffer-forward (- count))))

;;;###autoload
(defun deck-swap-buffer-forward (&optional count)
  "Swap active buffer with that in another window.

Display this buffer COUNT forward (in the same ordering as
`other-window'), skipping windows dedicated to their current
buffers, and display in this window the buffer that was displayed
there.

As a special case, if COUNT is zero, act as if COUNT were 1 and
then select the targeted window (as `deck-surf-swap-buffer-forward')."
  (interactive "p")
  (if (zerop count) (deck-surf-swap-buffer-forward 1)
    (deck--swap-buffers (selected-window)
                        (bfw-get-nth-window-not-dedicated count))))

;;;###autoload
(defun deck-swap-buffer-backward (&optional count)
  "As `deck-swap-buffer-forward' with direction reversed."
  (interactive "p")
  (if (zerop count) (deck-surf-swap-buffer-forward -1)
    (deck-swap-buffer-forward (- count))))

;;;###autoload
(defun deck-surf-swap-buffer-forward (&optional count)
  "As `deck-swap-buffer-forward' but select target window.

As a special case, if COUNT is zero, act as if COUNT were 1 but
do not select the target window.  This is for convenience in
interactive usage."
  (interactive "p")
  (if (zerop count) (deck-swap-buffer-forward 1)
    (let ((win (bfw-get-nth-window-not-dedicated count)))
      (deck--swap-buffers (selected-window) win)
      (select-window win))))

;;;###autoload
(defun deck-surf-swap-buffer-backward (&optional count)
  "As `deck-swap-buffer-backward' but select target window.

As a special case, if COUNT is zero, act as if COUNT were 1 but
do not select the target window.  This is for convenience in
interactive usage."
  (interactive "p")
  (if (zerop count) (deck-swap-buffer-forward -1)
    (deck-surf-swap-buffer-forward (- count))))

(provide 'deck)
;;; deck.el ends here
