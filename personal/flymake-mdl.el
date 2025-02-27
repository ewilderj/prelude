;; -*- lexical-binding: t; -*-
(use-package flymake-markdownlint-cli2
  :vc (:url "https://github.com/ewilderj/flymake-markdownlint-cli2"
            :rev :newest
            :branch "main")
  :config
  (add-hook 'markdown-mode-hook 'flymake-mode)
  (add-hook 'markdown-mode-hook 'flymake-markdownlint-cli2-setup)
  (setq flymake-markdownlint-cli2-config "/Users/ewilderj/etc/github.markdownlint-cli2.mjs"))

;; sets personal config, which takes advantage of the fact that extensions
;; are installed for formatting GitHub markdown...
;; $ npm install -D @github/markdownlint-github

(setq markdown-command '("/Users/ewilderj/bin/gh-markdown.sh"))

;; gh-markdown.sh wraps cmark_gfm:

;; # Path to the cmark-gfm executable
;; CMARK_GFM="/Users/ewilderj/bin/cmark-gfm"

;; # Extensions to enable
;; EXTENSIONS="-e table -e strikethrough -e autolink -e tasklist -e tagfilter"

;; $CMARK_GFM $EXTENSIONS "$@"
