;; quick access to a shell when we need it
;; https://github.com/amno1/emacs-term-toggle
(define-key global-map [S-f2] #'term-toggle-ansi)
(define-key global-map [f2] #'term-toggle-eshell)
