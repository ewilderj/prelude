;;; -*- lexical-binding: t; -*-

;;; Dev/Stable mode detection
;; When ~/git/agent-shell exists, use local git checkouts for development.
;; Otherwise, use stable ELPA versions.
(defvar ewj/agent-shell-dev-p
  (file-directory-p "~/git/agent-shell")
  "Non-nil when using local git checkout of agent-shell for development.")

(defvar ewj/shell-maker-dev-p
  (file-directory-p "~/git/shell-maker")
  "Non-nil when using local git checkout of shell-maker for development.")

(defvar ewj/acp-el-dev-p
  (file-directory-p "~/git/acp.el")
  "Non-nil when using local git checkout of acp.el for development.")

;;; Load shell-maker (dependency of agent-shell)
(if ewj/shell-maker-dev-p
    (progn
      (add-to-list 'load-path (expand-file-name "~/git/shell-maker"))
      (require 'shell-maker))
  (use-package shell-maker :ensure t))

;;; Load acp.el (dependency of agent-shell)
(if ewj/acp-el-dev-p
    (progn
      (add-to-list 'load-path (expand-file-name "~/git/acp.el"))
      (require 'acp))
  (use-package acp :ensure t))

;;; Load agent-shell
(if ewj/agent-shell-dev-p
    (progn
      (add-to-list 'load-path (expand-file-name "~/git/agent-shell"))
      (require 'agent-shell)
      (setq agent-shell-github-command
            '("copilot" "--yolo" "--acp" "--model" "claude-opus-4.6")))
  (use-package agent-shell
    :ensure t
    :demand t
    :ensure-system-package ()
    :custom
    (agent-shell-header-style 'text)
    (agent-shell-github-command
     (if (file-exists-p "/Users/ewilderj/git/copilot-agent-runtime-test/dist-cli/index.jsFOO")
         '("node" "/Users/ewilderj/git/copilot-agent-runtime-test/dist-cli/index.js" "--acp" "--model" "claude-opus-4.5")
       '("copilot" "--yolo" "--acp" "--model" "claude-opus-4.6")))))

;;; Reload command for development
(defun ewj/reload-agent-shell ()
  "Reload agent-shell and shell-maker from load-path.
Useful during development to pick up changes without restarting Emacs."
  (interactive)
  (when (featurep 'agent-shell)
    (unload-feature 'agent-shell t))
  (when (featurep 'acp)
    (unload-feature 'acp t))
  (when (featurep 'shell-maker)
    (unload-feature 'shell-maker t))
  (when ewj/shell-maker-dev-p
    (load (expand-file-name "~/git/shell-maker/shell-maker.el")))
  (when ewj/acp-el-dev-p
    (load (expand-file-name "~/git/acp.el/acp.el")))
  (when ewj/agent-shell-dev-p
    (load (expand-file-name "~/git/agent-shell/agent-shell.el")))
  (unless (or ewj/agent-shell-dev-p ewj/shell-maker-dev-p)
    (require 'agent-shell))
  ;; Re-apply customizations
  (ewj/agent-shell-apply-config)
  (message "Reloaded agent-shell%s"
           (if ewj/agent-shell-dev-p " (dev mode)" "")))

(defun ewj/agent-shell-apply-config ()
  "Apply agent-shell customizations. Called after loading/reloading."
  ;; Shorten the Copilot prompt from "Copilot> " to " ❯ "
  (setq agent-shell-agent-configs
        (mapcar (lambda (config)
                  (when (eq (alist-get :identifier config) 'copilot)
                    (setf (alist-get :shell-prompt config) " ❯ ")
                    (setf (alist-get :shell-prompt-regexp config) " ❯ "))
                  config)
                (agent-shell--make-default-agent-configs))))

;; Apply config on initial load
(ewj/agent-shell-apply-config)

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

;;; ELPA-only patches
;; These advices are only applied when using ELPA packages.
;; When developing upstream, implement the features directly in the code.

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

;; Only apply patches when using ELPA (not dev mode)
(unless ewj/agent-shell-dev-p
  (advice-add 'agent-shell--update-fragment :around #'my/agent-shell-show-command)
  (advice-add 'agent-shell--status-label :around #'my/agent-shell-status-icons)
  (advice-add 'agent-shell-make-tool-call-label :around #'my/agent-shell-kind-icons))
