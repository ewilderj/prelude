;; customization to the minibuf and echo area

;; use the condensed font from the modeline for minibuf area too
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       `((default :family ,ewilderj-condense-font :height 130))))

(defun my-echo-area-setup (&optional _frame)
  "Apply font settings to the echo area buffer."
  ;; We check if the buffer exists to be safe
  (let ((echo-buf (get-buffer " *Echo Area 0*")))
    (when echo-buf
      (with-current-buffer echo-buf
        (setq-local face-remapping-alist
                    `((default :family ,ewilderj-condense-font :height 130)))))))
;; Add to the hook so it runs every time a new client window is created
(add-hook 'after-make-frame-functions #'my-echo-area-setup)

;; If we ever start Emacs normally (not as daemon), run it once immediately
(unless (daemonp)
  (my-echo-area-setup))
