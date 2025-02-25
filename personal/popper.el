;; -*- lexical-binding: t; -*-
(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '(("\\*Messages\\*" . hide)
          ("Output\\*$" . hide)
          ("Warnings" . hide)
          "\\*Async Shell Command\\*"
          "vc\-"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(setq popper-display-control t)

(custom-set-variables
 '(popper-mode-line '(:eval (propertize "🌶️ " 'face 'mode-line-emphasis))))
