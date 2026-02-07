;;; -*- lexical-binding: t; -*-


(use-package agent-shell
  :ensure t
  :demand t
  :ensure-system-package
  ;; Add agent installation configs here
  ()
  :custom
  (agent-shell-github-command
   (if (file-exists-p "/Users/ewilderj/git/copilot-agent-runtime-test/dist-cli/index.jsFOO")
       '("node" "/Users/ewilderj/git/copilot-agent-runtime-test/dist-cli/index.js" "--acp" "--model" "claude-opus-4.5")
     '("copilot" "--yolo" "--acp" "--model" "claude-opus-4.6" )))
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
(setq markdown-overlays-insert-dividers t)

(use-package markdown-tables
  :after markdown-overlays
  :load-path "personal/lib"
  :config
  (markdown-tables-enable))

(use-package markdown-mermaid
  :after markdown-overlays
  :load-path "personal/lib"
  :config
  (markdown-mermaid-enable))

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

;; PATCH: Replace verbose status labels with icons
(defun my/agent-shell-status-icons (orig-fn status)
  "Replace verbose status labels with compact icons."
  (let* ((config (pcase status
                   ("pending" '("◌" font-lock-comment-face))
                   ("in_progress" '("◐" warning))
                   ("completed" '("✓" success))
                   ("failed" '("✗" error))
                   (_ '("?" warning))))
         (icon (car config))
         (face (cadr config)))
    (propertize (format " %s " icon) 'font-lock-face face)))

(advice-add 'agent-shell--status-label :around #'my/agent-shell-status-icons)

;; PATCH: Replace verbose kind labels with icons
;; Spec kinds: read, edit, delete, move, search, execute, think, fetch, switch_mode, other
;; See: https://docs.rs/agent-client-protocol/latest/agent_client_protocol/enum.ToolKind.html
(defun my/agent-shell-kind-icons (orig-fn state tool-call-id)
  "Replace verbose kind labels with compact icons in tool-call labels."
  (let ((result (funcall orig-fn state tool-call-id)))
    (when result
      (when-let* ((tool-call (map-nested-elt state `(:tool-calls ,tool-call-id)))
                  (kind (map-elt tool-call :kind)))
        (let ((icon (pcase kind
                      ("read" "📖")
                      ("edit" "✏️")
                      ("delete" "🗑")
                      ("move" "📦")
                      ("search" "🔍")
                      ("execute" "⚡")
                      ("think" "💭")
                      ("fetch" "🌐")
                      ("switch_mode" "🔀")
                      ("other" "🔧")
                      (_ nil))))
          (when (and icon (alist-get :status result))
            (setf (alist-get :status result)
                  (concat
                   (when-let ((status (map-elt tool-call :status)))
                     (my/agent-shell-status-icons nil status))
                   icon))))))
    result))

(advice-add 'agent-shell-make-tool-call-label :around #'my/agent-shell-kind-icons)
