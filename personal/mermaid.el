;; mermaid.el --- Mermaid diagram support in org-mode  -*- lexical-binding: t; -*-

;; Requires: brew install mermaid-cli

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
