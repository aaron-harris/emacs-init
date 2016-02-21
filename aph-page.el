;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; PAGE EXTENSIONS
;;;;============================================================================

;; Extensions for `page' package.
(require 'page)
(require 'hydra)


;;; Page Motion Commands
;;;=====================
(defhydra aph/hydra-page (:color red)
  "Page motion"
  ("]"        forward-page  "forward")
  ("["        backward-page "back")
  ("<return>" nil           "quit" :color blue))


(provide 'aph-page)
