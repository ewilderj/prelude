(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))

(add-hook 'prog-mode-hook 'copilot-mode)


(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(add-to-list 'copilot-major-mode-alist '("enh-ruby" . "ruby"))

(use-package copilot-chat
    :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
    :after (request org markdown-mode shell-maker))

;; I think popper makes this bothersome, so turn it off
(setq copilot-indent-offset-warning-disable t)
(setq copilot-max-char-warning-disable t)
