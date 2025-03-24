;; graphql-mode -*-lexical-binding: t; -*-

;; everything we need for graphql fun and games
(use-package graphql-mode
  :ensure t
  :mode "\\.graphql\\'")

(use-package request
  :ensure t)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")
