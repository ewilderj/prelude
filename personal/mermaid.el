;; mermaid.el --- Mermaid diagram support in org-mode  -*- lexical-binding: t; -*-

;; Requires: brew install mermaid-cli

;; Puppeteer (used by mmdc) needs Chrome - set path for GUI Emacs
(setenv "PUPPETEER_EXECUTABLE_PATH"
        "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

(defun ewj/check-mmdc-installed ()
  "Check that mmdc (mermaid-cli) is available."
  (unless (executable-find "mmdc")
    (user-error "mmdc not found. Install with: brew install mermaid-cli")))

(use-package mermaid-mode
  :ensure t
  :config
  (ewj/check-mmdc-installed)
  (setq mermaid-mmdc-location (executable-find "mmdc")))

(use-package ob-mermaid
  :ensure t
  :after (org mermaid-mode)
  :config
  (setq ob-mermaid-cli-path (executable-find "mmdc"))
  (add-to-list 'org-babel-load-languages '(mermaid . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; Display images inline after babel execution
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Don't prompt for confirmation on safe languages
(add-to-list 'org-babel-default-header-args:mermaid '(:eval . "yes"))
(setq org-confirm-babel-evaluate
      (lambda (lang _body)
        (not (string= lang "mermaid"))))


;; Tree-sitter markdown grammar (used by md-mermaid-live for fence detection)
(add-to-list 'treesit-language-source-alist
             '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                        "split_parser" "tree-sitter-markdown/src"))
(unless (treesit-language-available-p 'markdown)
  (treesit-install-language-grammar 'markdown))

;; md-mermaid: live overlay rendering in markdown buffers
;; LOCAL FIX: md-mermaid-live--ts-build-block has a bug where :pos uses
;; code-end (last code line) instead of block-end (closing fence line),
;; causing tree-sitter renders to always be marked "stale".
;; Fix: in md-mermaid-live.el, remove the `pos` let-binding and set
;; :pos to block-end.
;; LOCAL FIX: md-mermaid-live--ensure-overlay fails to reuse overlays
;; after edits that shift buffer positions (stored md-mermaid-pos goes
;; stale). Fix: add (= (overlay-start o) pos) fallback to the
;; seq-find match in ensure-overlay.
;; See https://github.com/ahmetus/md-mermaid/issues/1
(use-package md-mermaid
  :straight (:host github :repo "ahmetus/md-mermaid"
             :files ("*.el" "scripts"))
  :commands (md-mermaid-render-current
             md-mermaid-transient
             md-mermaid-live-mode)
  :init
  (md-mermaid-keybindings-mode 1)
  :config
  (setq md-mermaid-default-preset 'png1400)
  ;; Match diagram background to Emacs theme
  (setq md-mermaid-live-background (face-background 'default)))
