;;; -*- lexical-binding: t; -*-


(use-package agent-shell
  :ensure t
  :demand t
  :ensure-system-package
  ;; Add agent installation configs here
  ()
  :custom
  (agent-shell-github-command '("/Users/ewilderj/git/copilot-agent-runtime-test/dist-cli/index.js" "--acp" "--model" "claude-opus-4.5"))
;;  (agent-shell-github-command '("copilot" "--acp" "--model" "claude-opus-4.5"))
  :config
  ;; Shorten the Copilot prompt from "Copilot> " to " ❯ "
  (defun my/shorten-copilot-prompt (config)
    "Replace Copilot prompt with a shorter one."
    (when (eq (alist-get :identifier config) 'copilot)
      (setf (alist-get :shell-prompt config) " ❯ ")
      (setf (alist-get :shell-prompt-regexp config) " ❯ "))
    config)
  (advice-add 'agent-shell-github-make-copilot-config :filter-return #'my/shorten-copilot-prompt))

;; Markdown table alignment for agent-shell output

(load-file "~/.emacs.d/personal/lib/markdown-overlays.el")

;;(use-package markdown-tables
;;  :after markdown-overlays
;;  :load-path "personal/lib"
;;  :config
;;  (markdown-tables-enable))
