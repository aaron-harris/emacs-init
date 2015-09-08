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
(defvar aph/org-capture-in-popout-frame t
  "If non-nil, `org-capture' does its work in a new frame.")

;; What we need to do:
;; - Make `org-capture-select-template' use a pop-up frame,
;;   and then keep it around.
;; --> `org-capture-select-template' delegates to
;;     `org-switch-to-buffer-other-window', so that's what we need to
;;     advise.
;; - Probably need to fix the value stored as :return-to-wconf.
;; - Make `org-capture-fill-template' reuse the same capture frame.

(defun aph/org-switch-to-buffer-in-capture-frame (&rest args)
  "Advice to support `aph/org-capture-in-popout-frame'.
When this variable is non-nil, override the usual behavior of
`org-switch-to-buffer-other-window' and open the buffer described
by ARGS in a frame named \"Capture\", creating such a frame if
necessary.

Otherwise, return nil to return control to
`org-switch-to-buffer-other-window'."
  (when aph/org-capture-in-popout-frame 
    (apply #'switch-to-buffer-other-frame args)))

(advice-add #'org-switch-to-buffer-other-window
            :before-until
            #'aph/org-switch-to-buffer-in-capture-frame)

;; To clean up WIP code:
(advice-remove #'org-switch-to-buffer-other-window 
               #'aph/org-switch-to-buffer-in-capture-frame)

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

;; WIP Cleanup
(setq display-buffer-alist)
(advice-remove #'org-switch-to-buffer-other-window
               #'pop-to-buffer)
