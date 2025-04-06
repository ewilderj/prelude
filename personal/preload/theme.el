;; (setq prelude-theme 'sanityinc-tomorrow-eighties)
;; (setq prelude-theme 'solarized-dark)

(use-package dracula-theme
  :ensure t
  :demand t
  :straight (:host github :repo "dracula/emacs" :files ("*.el"))
  :config
  (setq prelude-theme 'dracula))

;; (use-package autothemer
;;   :ensure t)
;; (use-package rose-pine-theme
;;   :after autothemer
;;   :ensure t
;;   :vc (:url "https://github.com/konrad1977/pinerose-emacs"
;;             :rev :newest
;;             :branch "main"))
;; (setq prelude-theme 'rose-pine)
