;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; FRAME EXTENSIONS
;;;;============================================================================

;;; Extensions for `frame' module.
(require 'frame)


;;; Frame Geometry
;;;===============
;; Many of the functions in this section involve correcting other
;; functions for use on Windows systems, which do not always seem to
;; return the correct values.

(defvar aph/frame-width-correction 16
  "The signed error in pixels for `frame-pixel-width'.
For some reason, on Windows systems `frame-pixel-width' reports
the wrong window width.  This value is subtracted from the result
returned by `frame-pixel-width' to obtain the correct value; see
`aph/frame-true-width'.")

(defvar aph/frame-margin-width 0
  "The width in pixels of the window chrome on a frame.
This refers to the amount of space on the left or right side of a
frame, outside the usable area.")

(defvar aph/frame-offset 0
  "The difference between the origin and the screen's left side.

For some reason on some systems the logical origin used for frame
positioning is visibly different from the upper left-hand corner
of the screen.  This is the horizontal difference between the two
points.")

(defvar aph/frame-w32-taskbar-height 33
  "The height of the Windows taskbar in pixels.")

(defun aph/frame-monitor-width (&optional frame)
  "Return the width in pixels of the monitor dominating FRAME.
If FRAME is omitted or nil, use the selected frame."
  (let* ((monitor  (cdr (assq 'geometry (frame-monitor-attributes frame))))
         (left     (nth 0 monitor))
         (right    (nth 2 monitor)))
    (- right left)))

(defun aph/frame-fullscreen-width (&optional frame)
  "Return the width in pixels FRAME would need to fill its monitor.
This is the value returned by `aph/frame-monitor-width', less the
width of the window chrome for the system in use.

As with `aph/frame-monitor-width', if DISPLAY is omitted or nil,
it defaults to the selected frame's display." 
  (- (aph/frame-monitor-width frame) (* 2 aph/frame-margin-width)))

(defun aph/frame-true-width (&optional frame)
  "Return FRAME's actual width in pixels.
This is the value returned by `frame-pixel-width', less the value
of `aph/frame-width-correction'.  It is unknown why
`frame-pixel-width' does not appear to return this value in the
first place.

After executing (set-frame-width FRAME X nil :pixelwise), this
function will return X, while `frame-pixel-width' will not.

As with `frame-pixel-width', if FRAME is omitted or nil, it
defaults to the selected frame." 
  (- (frame-pixel-width frame) aph/frame-width-correction))

(provide 'aph-frame)
