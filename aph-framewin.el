;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; FRAME AND WINDOW FUNCTIONS
;;;;============================================================================

;;; This file contains functions dealing with frames and windows.


;;; Named Frames
;;;=============
;; These functions provide support for dealing with named frames.
(defun aph/get-frame-by-name (fname)
  "If there is a frame named FNAME, return it, else nil."
  (require 'dash)                       ; For `-some'
  (-some (lambda (frame)
           (when (equal fname (frame-parameter frame 'name))
             frame))
         (frame-list)))

(defun aph/display-buffer-in-named-frame (buffer alist)
  "Display BUFFER in frame with specific name.
The name to use is the value associated with the 'named-frame key
in ALIST.  If a frame with that name already exists, use it.
Otherwise, call `display-buffer-in-pop-up-frame' to create it.

If ALIST does not contain the key 'named-frame, use the name of BUFFER."
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

(provide 'aph-framewin)
