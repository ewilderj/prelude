;; -*- lexical-binding: t; -*-

(setq org-gtd-update-ack "3.0.0")

(use-package org-gtd
  :after org
  :demand t
  :custom
    (org-gtd-directory "~/org/gtd")
    (org-edna-use-inheritance t)
  :config
  (org-edna-mode)
  ;; Add custom keybindings for GTD functions
  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-show-stuck-projects)
   :map org-gtd-process-map
   ("C-c c" . org-gtd-choose)))
