(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

(use-package elisp-dev-mcp
  :ensure t
  :after server
  :custom
  (elisp-dev-mcp-additional-allowed-dirs
  '("~/.emacs.d"))
  :config
  (mcp-server-lib-start))

;; use this in .vscode/mcp.json
;;
;; "elisp-dev-mcp": {
;;   "type": "stdio",
;;   "command": "~/.emacs.d/emacs-mcp-stdio.sh",
;;   "args": [
;;      "--init-function=elisp-dev-mcp-enable",
;;      "--stop-function=elisp-dev-mcp-disable",
;;      "--server-id=elisp-dev-mcp"
;;   ]
;; }
