;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; FRAME AND WINDOW FUNCTIONS
;;;;============================================================================

;;; This file contains functions dealing with frames and windows.


;;; Cyclic Window Selection
;;;========================
(defun aph/get-nth-window-with-predicate
    (n predicate &optional minibuf all-frames default)
  "As `get-window-with-predicate', but return the Nth success.

If N is positive, return the Nth window forward in the cyclic
ordering of windows that is not dedicated to its current buffer.
If N is negative, instead return the (- N)th window backwards.
If N is zero, return selected window if it is not dedicated and
DEFAULT otherwise (or nil if DEFAULT is not supplied).

The parameters MINIBUF, ALL-FRAMES, and DEFAULT have the same
meanings as in `get-window-with-predicate'."
  (cond
   ((null (get-window-with-predicate predicate minibuf all-frames))
    default)
   ((= n 0)
    (if (funcall predicate (selected-window))
        (selected-window)
      default))
   (t
    (let*  ((cycler  (cond
                      ((> n 0) #'next-window)
                      ((< n 0) #'previous-window)))
            (cursor  (selected-window))
            (i        (abs n)))
      (while (> i 0)
        (setq cursor (funcall cycler cursor minibuf all-frames))
        (when (funcall predicate cursor)
          (setq i (1- i))))
      cursor))))

(defun aph/get-nth-window-not-dedicated
    (n &optional minibuf all-frames default)
  "Return the Nth window forward that is not dedicated.

If N is positive, return the Nth window forward in the cyclic
ordering of windows that is not dedicated to its current buffer.
If N is negative, instead return the (- N)th window backwards.
If N is zero, return selected window if it is not dedicated and
DEFAULT otherwise (or nil if DEFAULT is not supplied).

The parameters MINIBUF, ALL-FRAMES, and DEFAULT have the same
meanings as in `get-window-with-predicate'."
  (aph/get-nth-window-with-predicate
   n
   (lambda (win) (not (window-dedicated-p win)))
   minibuf all-frames default))


;;; Buffer Control
;;;===============
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
;;;###autoload
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
                      (aph/get-nth-window-not-dedicated count) ride)))

;;;###autoload
(defun aph/slide-buffer-backward (&optional count ride)
  "As `aph/slide-buffer-forward' with direction reversed."
  (interactive "p")
  (if (zerop count) (aph/slide-buffer-forward -1 :ride)
    (aph/slide-buffer-forward (- count) ride)))

;;;###autoload
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
    (let* ((win (aph/get-nth-window-not-dedicated count)) 
           (buf (window-buffer win)))
      (aph/swap-buffers (selected-window) win)
      (when ride (select-window win)))))

;;;###autoload
(defun aph/swap-buffer-backward (&optional count ride)
  "As `aph/swap-buffer-forward' with direction reversed."
  (interactive "p")
  (if (zerop count) (aph/swap-buffer-forward -1 :ride)
    (aph/swap-buffer-forward (- count) ride)))

;;;###autoload
(defun aph/swap-buffer-forward-and-ride (&optional count)
  "As `aph/swap-buffer-forward' but always ride."
  (interactive "p")
  (aph/swap-buffer-forward count :ride))

;;;###autoload
(defun aph/swap-buffer-backward-and-ride (&optional count)
  "As `aph/swap-buffer-backward' but always ride."
  (interactive "p")
  (aph/swap-buffer-backward count :ride))

;;;###autoload
(defun aph/pull-buffer-backward (&optional count)
  "Pull buffer from another window.

Display in this buffer the one currently displayed in the window
COUNT windows forward (in the same ordering as `other-window'),
skipping windows dedicated to their current buffers.  Display in
the window previously occupied by this buffer the previous buffer
displayed in that window (using `switch-to-prev-buffer')."
  (interactive "p")
  (aph/slide-buffer (aph/get-nth-window-not-dedicated count)
                    (selected-window)))

;;;###autoload
(defun aph/pull-buffer-forward (&optional count)
  "As `aph/pull-buffer-backward' with direction reversed."
  (interactive "p")
  (aph/pull-buffer-backward (- count)))


;;; Named Frames
;;;=============
;; This function provides support for dealing with named frames.
(defun aph/get-frame-by-name (fname)
  "If there is a frame named FNAME, return it, else nil."
  (require 'dash)                       ; For `-some'
  (-some (lambda (frame)
           (when (equal fname (frame-parameter frame 'name))
             frame))
         (frame-list)))


;;; `display-buffer-alist' Functions
;;;=================================
;; These functions are intended for use with `display-buffer-alist'.
;; They allow precise control over how to display buffers.

(defun aph/display-buffer-in-named-frame (buffer alist)
  "Display BUFFER in frame with specific name.
The name to use is the value associated with the 'named-frame key
in ALIST.  If a frame with that name already exists, use it.
Otherwise, call `display-buffer-in-pop-up-frame' to create it.

If ALIST does not contain the key 'named-frame, use the name of
BUFFER."
  (let* ((fname  (or (cdr (assq 'named-frame alist))
                     (buffer-name buffer)))
         (frame  (aph/get-frame-by-name fname)))
    (if frame
        (window--display-buffer buffer
                                (frame-selected-window frame)
                                'reuse) 
      (display-buffer-pop-up-frame
       buffer
       (add-to-list 'alist `(pop-up-frame-parameters
                             (name . ,fname)))))))

(defun aph/display-buffer-in-subwindow (buffer alist)
  "Display BUFFER in a small split window.

Split the current window, with the new window being small (5
lines high) and below the original, and display BUFFER there.

The second parameter ALIST is ignored and exists only so this
function can be used in `display-buffer-alist'."
  (let ((new-win (split-window (selected-window) -5 'below)))
    (set-window-buffer new-win buffer)
    new-win))

(provide 'aph-framewin)
