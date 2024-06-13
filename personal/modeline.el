(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font")
  )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(custom-set-faces
 '(mode-line ((t (:family "Zed Mono" :height 130))))
 '(mode-line-active ((t (:family "Zed Mono" :height 130)))) ; For 29+
 '(mode-line-inactive ((t (:family "Zed Mono" :height 130)))))

;; this just takes up space
(setq doom-modeline-lsp-icon nil)

;; not usually useful these days
(setq doom-modeline-buffer-encoding t)

;; we don't need to see buffer size when we have line numbering
(size-indication-mode -1)
