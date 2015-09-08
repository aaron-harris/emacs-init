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


;;; Make capture occur in new frame
;;;==================================
;; A new approach: This stackexhange thread discusses use of
;; `display-buffer-alist':
;; http://emacs.stackexchange.com/questions/2194/how-do-i-force-a-specific-buffer-to-open-in-a-new-window
;; This may be a more promising approach.  See also documentation for
;; `display-buffer'.

;; The first thing to do is to find or write a function that displays
;; a buffer in a named frame, creating the frame if it does not exist.
(defun aph/get-frame-by-name (fname)
  "If there is a frame with NAME, return it, else nil."
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
      ;; TODO: Modify alist to make sure it includes the name.
      (display-buffer-pop-up-frame
       buffer
       (add-to-list 'alist `(pop-up-frame-parameters
                             (name . ,fname)))))))

(setq display-buffer-alist
      '(("\\*Org Select\\*\\|\\*Capture\\*\\|CAPTURE-.*"
         aph/display-buffer-in-named-frame
         (named-frame . "Capture"))))

;; Problems still to be addressed with the new approach.:
;; - The *Org Select* buffer is still not showing up in the frame.
;; - When the *Capture* buffer is up, all but one of the windows in
;;   the original frame have been deleted (though they are restored
;;   properly at the end of the process)
;; - The CAPTURE-... buffer is also not showing up in the frame.
;; - The capture frame sticks around after the capture process is
;;   complete.

(advice-add #'org-switch-to-buffer-other-window
            :override
            #'pop-to-buffer)

;; Progress!  All buffers now appear in their proper places, and we
;; just need to resolve extraneous changes to the background window
;; configuration.

(defun aph/org-capture-in-popout-frame (&optional goto keys)
  "As `org-capture', but do all work in a new frame."
  (interactive "P")
  (let ((override  '("\\*Org Select\\*\\|\\*Capture\\*\\|CAPTURE-.*"
                     aph/display-buffer-in-named-frame
                     (named-frame . "Capture"))))
    ;; This makes all relevant buffers open in a specific capture frame.
    (add-to-list 'display-buffer-alist override)
    ;; This makes Org-mode obey `display-buffer-alist'.
    (advice-add #'org-switch-to-buffer-other-window :override #'pop-to-buffer)
    ;; This stops Org-mode from messing with our window configuration.
    (advice-add #'delete-other-windows              :override #'ignore) 
    (org-capture goto keys)
    (setq display-buffer-alist (delete override display-buffer-alist))
    (advice-remove #'org-switch-to-buffer-other-window  #'pop-to-buffer)
    (advice-remove #'delete-other-windows               #'ignore)))

;; Almost done.  Now we just need to make finalizing or aborting
;; capture delete the Capture frame.

;; Also, maybe we should consider writing a "temporary advice" macro?
