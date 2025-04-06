;; -*- lexical-binding: t; -*-
(use-package flymake-markdownlint-cli2
  :ensure t 
  :vc (:url "https://github.com/ewilderj/flymake-markdownlint-cli2"
            :rev :newest
            :branch "main")
  :config
  (add-hook 'markdown-mode-hook 'flymake-mode)
  (add-hook 'markdown-mode-hook 'flymake-markdownlint-cli2-setup)
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (let ((home-dir (expand-file-name "~")))
    (setq flymake-markdownlint-cli2-config
          (concat home-dir "/etc/github.markdownlint-cli2.mjs"))
    (setq markdown-command
          (concat home-dir "/bin/gh-markdown.sh"))))

;; sets personal config, which takes advantage of the fact that extensions
;; are installed for formatting GitHub markdown...
;; $ npm install -D @github/markdownlint-github

;; gh-markdown.sh wraps cmark_gfm:

;; # Path to the cmark-gfm executable
;; CMARK_GFM="/Users/ewilderj/bin/cmark-gfm"

;; # Extensions to enable
;; EXTENSIONS="-e table -e strikethrough -e autolink -e tasklist -e tagfilter"

;; $CMARK_GFM $EXTENSIONS "$@"
