;; customization to the minibuf and echo area

;; use the condensed font from the modeline for minibuf area too
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :family "Zed Mono" :height 130))))

;; and for echo area, which shares space with the minibuf
(with-current-buffer (get-buffer " *Echo Area 0*")
  (setq-local face-remapping-alist '((default :family "Zed Mono" :height 130))))
