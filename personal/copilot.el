;; Copilot configuration		 -*- lexical-binding: t; -*-
(defun my-check-copilot-server-installed ()
  (condition-case err
      (progn
        (if (file-exists-p (copilot-server-executable)) t
          nil))
      (error
       (message "Error enabling copilot mode '%s'" err)
       nil)))

(use-package copilot
  :ensure t
  :demand t
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :after (popper)
  :config
  (if (not (my-check-copilot-server-installed))
      (progn
        (copilot-install-server)
        (sleep-for 4)))
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (add-to-list 'copilot-major-mode-alist '("enh-ruby" . "ruby"))
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-max-char-warning-disable t))

(use-package copilot-chat
    :ensure t
    :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
    :after (copilot request org markdown-mode shell-maker)
)

;; I think popper makes this bothersome, so turn it off



;; (condition-case err
;;     (progn
;;       (message "Attempting to enable copilot-mode")
;;       (copilot-mode 1)
;;       )
;;   (error
;;    (message "Error enabling copilot mode '%s', trying to install server" err)
;;    (progn
;;      (copilot-install-server))))
