;;; -*- lexical-binding: t; -*-


(use-package agent-shell
  :ensure t
  :demand t
  :ensure-system-package
  ;; Add agent installation configs here
  ()
  :custom
  (agent-shell-github-command '("node" "/Users/ewilderj/git/copilot-agent-runtime-test/dist-cli/index.js" "--acp" "--model" "claude-opus-4.5"))
  ;; (agent-shell-github-command '("copilot" "--acp" "--model" "claude-opus-4.5"))
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

(use-package markdown-tables
  :after markdown-overlays
  :load-path "personal/lib"
  :config
  (markdown-tables-enable))

;; Disable line numbers in agent-shell buffers
(add-hook 'agent-shell-mode-hook (lambda () (display-line-numbers-mode -1)))

;; PATCH: Display command text in tool_call fragments
;; See: https://github.com/xenodium/agent-shell/issues/XXX
(defun my/agent-shell-show-command (orig-fn &rest args)
  "Advice to prepend command to tool-call fragment body."
  (let* ((state (plist-get args :state))
         (block-id (plist-get args :block-id))
         (body (plist-get args :body))
         (tool-calls (and state (map-elt state :tool-calls)))
         (command (and tool-calls block-id
                       (map-nested-elt tool-calls (list block-id :command)))))
    (when (and command body (not (string-empty-p command)))
      (setq args (plist-put args :body
                            (concat (propertize (format "$ %s\n\n" command)
                                                'font-lock-face 'font-lock-comment-face)
                                    body))))
    (apply orig-fn args)))

(advice-add 'agent-shell--update-fragment :around #'my/agent-shell-show-command)
