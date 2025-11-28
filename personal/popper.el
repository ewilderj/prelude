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
          ("\\*Warnings\\*" . hide)
          "\\*Async Shell Command\\*"
          ("\\*copilot-install-server\\*" . hide)
          "vc\-"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(setq popper-display-control t)
(setq warning-minimum-level :error)

(custom-set-variables
 '(popper-mode-line '(:eval (propertize "🌶️ " 'face 'mode-line-emphasis))))
