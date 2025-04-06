(use-package copilot
  :ensure t
  :demand t
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :config
  (condition-case err
      (progn
        (message "Attempting to enable copilot-mode")
        (copilot-mode 1)
        (add-hook 'prog-mode-hook 'copilot-mode)
        )
    (error
     (message "Error enabling copilot mode '%s', trying to install server" err)
     (progn
       (interactive)
       (copilot-install-server)
       )
     )
    )
)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(add-to-list 'copilot-major-mode-alist '("enh-ruby" . "ruby"))

(use-package copilot-chat
    :ensure t
    :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
    :after (copilot request org markdown-mode shell-maker)
)

;; I think popper makes this bothersome, so turn it off
(setq copilot-indent-offset-warning-disable t)
(setq copilot-max-char-warning-disable t)
