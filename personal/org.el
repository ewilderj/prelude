;; -*- lexical-binding: t; -*-

;; see straight.el for the main loading of org

(use-package ox-pandoc
  :after org
  :ensure t)

(use-package org-bullets
  :after org
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
