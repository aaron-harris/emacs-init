;;;; The Emacs init file of Aaron Harris.
;;;; MODE HOOKS
;;;;============================================================================

;; Enabling paredit in all lisp modes.
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)
(add-hook 'clojure-mode-hook                     #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook                  #'enable-paredit-mode)

;; Enabling Rainbow Delimiters in all lisp modes.
(add-hook 'emacs-lisp-mode-hook                  #'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook                        #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook                        #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook            #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook                      #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook                     #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook                  #'rainbow-delimiters-mode)

;; Enable subword mode in Clojure mode, and in the Cider REPL.
(add-hook 'clojure-mode-hook    #'subword-mode)
(add-hook 'cider-repl-mode-hook #'subword-mode)

;; Enabling auto-fill mode in all text modes.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Enabling eldoc mode in Emacs Lisp buffers.
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
