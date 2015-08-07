;;; -*- lexical-binding: t -*-

;;;; The Emacs init file of Aaron Harris.
;;;; VISIBLE MARK MODE CONFIGURATION
;;;;============================================================================

;;; Configuration adapted from this blog post:
;;; http://pragmaticemacs.com/emacs/regions-marks-and-visual-mark/

;; No idea how these faces work on a terminal.  At least
;; `visible-mark-active' should be defined before requiring
;; 'visible-mark.
(defface visible-mark-active
  '((t (:underline "magenta")))
  "Face for the active mark. Preempts default definition."
  :group 'visible-mark)

(defface aph/visible-mark-top
  '((t (:underline "light salmon")))
  "Face for the most recent inactive mark."
  :group 'visible-mark)

(defface aph/visible-mark-other
  '((t (:underline "light goldenrod")))
  "Face for marks other than the most recent."
  :group 'visible-mark)

(require 'visible-mark)
(global-visible-mark-mode 1)
(setq visible-mark-max 2)
(setq visible-mark-faces '(aph/visible-mark-top
                           aph/visible-mark-other))

(provide 'init-visible-mark)
