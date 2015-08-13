;;; -*- lexical-binding: t -*-

;;;; The Emacs init files of Aaron Harris:
;;;; SMARTPARENS CONFIGURATION
;;;;============================================================================ 

(require 'smartparens-config)
(smartparens-global-mode)


;; String Handling
;;================
(add-to-list 'sp-navigate-consider-stringlike-sexp 'org-mode)
(add-to-list 'sp-navigate-consider-stringlike-sexp 'lisp-mode)
(add-to-list 'sp-navigate-consider-stringlike-sexp 'clojure-mode)


;; Mode-Specific Tweaks
;;=====================
;; Smartparens isn't treating the minibuffer during M-: as a lisp mode, so we
;; need to disable the single-quote pair there.
(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;; Use `` instead of `' in Clojure mode.
(sp-local-pair 'clojure-mode "`" "`"
               :when '(sp-in-string-p sp-in-comment-p))

(provide 'init-smartparens)
