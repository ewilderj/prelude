;; theme configuration -*- lexical-binding: t; -*-

(use-package solarized-theme
  :ensure t
  :custom
  (prelude-theme 'solarized-dark)
  (solarized-use-variable-pitch nil))

;; (use-package dracula-theme
;;   :ensure t
;;   :demand t
;;   :straight (:host github :repo "dracula/emacs" :files ("*.el"))
;;   :custom
;;   (prelude-theme 'dracula))



;; (use-package autothemer
;;   :ensure t)
;; (use-package rose-pine-theme
;;   :after autothemer
;;   :ensure t
;;   :vc (:url "https://github.com/konrad1977/pinerose-emacs"
;;             :rev :newest
;;             :branch "main"))
;; (setq prelude-theme 'rose-pine)
