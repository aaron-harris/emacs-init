;;;; The Emacs init files of Aaron Harris:
;;;; PACKAGE MANAGEMENT
;;;;============================================================================

(require 'package)
(setq package-archives '(("elpa"      . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))
(package-initialize)

(provide 'init-package)
