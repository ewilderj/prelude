;; theme configuration -*- lexical-binding: t; -*-

;; (use-package solarized-theme
;;   :ensure t
;;   :custom
;;   (prelude-theme 'solarized-light)
;;   (solarized-use-variable-pitch nil))

;; (use-package material-theme
;;   :ensure t
;;   :custom
;;   (prelude-theme 'material-light))

;; (use-package monokai-theme
;;   :ensure t
;;   :custom
;;   (prelude-theme 'monokai))

;; (use-package dracula-theme
;;   :ensure t
;;   :demand t
;;   :straight (:host github :repo "dracula/emacs" :files ("*.el"))
;;   :custom
;;   (prelude-theme 'dracula))

(add-to-list 'custom-theme-load-path prelude-personal-dir)

;; Use dracula-light in graphical mode, regular dracula in terminal
;; Must be deferred for server mode (no frame exists at preload time)
(defun ewilderj-setup-theme (&optional frame)
  "Set theme based on display type. Works in both daemon and normal startup."
  (when frame (select-frame frame))
  ;; Disable any existing themes to prevent face inheritance issues
  (mapc #'disable-theme custom-enabled-themes)
  (if (display-graphic-p)
      (progn
        (setq dracula-light-bg "#FAF9F5") ;; Tone down the creaminess
        (setq dracula-light-comment "#6272A4") ;; Blue-grey comments
        (setq prelude-theme 'dracula-light)
        (load-theme 'dracula-light t))
    (use-package dracula-theme
      :ensure t
      :straight (:host github :repo "dracula/emacs" :files ("*.el")))
    (setq prelude-theme 'dracula)
    (load-theme 'dracula t)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'ewilderj-setup-theme)
  (ewilderj-setup-theme))


;; (use-package dracula-light-theme
;;  :load-path "personal"
;;  :ensure nil
;;  :custom
;;  (prelude-theme 'dracula-light))

;; (use-package autothemer
;;   :ensure t)
;; (use-package rose-pine-theme
;;   :after autothemer
;;   :ensure t
;;   :vc (:url "https://github.com/konrad1977/pinerose-emacs"
;;             :rev :newest
;;             :branch "main"))
;; (setq prelude-theme 'rose-pine)
