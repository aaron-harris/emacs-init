;;;; The Emacs init files of Aaron Harris:
;;;; GNUS CONFIGURATION
;;;;============================================================================

;;; THIS FILE IS STILL VERY MUCH UNDER CONSTRUCTION!

(require 'gnus)

(setq nnrss-directory "~/news/rss/")
(setq gnus-select-method '(nnnil ""))
(setq gnus-group-line-format "%M%S%p%P%5y:%B%(%G%)\n")

;; Open links within RSS entries in Eww.
;; Retain original binding at M-<RET>.
(eval-after-load "gnus"
  (progn
    #'(define-key gnus-article-mode-map
        (kbd "<RET>") 'eww-browse-url)
    #'(define-key gnus-article-mode-map
        (kbd "M-<RET>") 'shr-browse-url)))

