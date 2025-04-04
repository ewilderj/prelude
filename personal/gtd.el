;; -*- lexical-binding: t; -*-

;; use-package org-gtd
(use-package org-gtd
  :ensure t
  :after org
  :config
  (setq org-gtd-directory "~/org/gtd")
  (setq org-gtd-inbox-file (concat org-gtd-directory "/inbox.org"))
  (setq org-gtd-projects-file (concat org-gtd-directory "/projects.org"))
  (setq org-gtd-refile-file (concat org-gtd-directory "/refile.org"))
  (setq org-gtd-archive-file (concat org-gtd-directory "/archive.org"))
  (setq org-gtd-tickler-file (concat org-gtd-directory "/tickler.org"))
  (setq org-gtd-templates
        '(("t" "Task" entry
           (file+headline "Tasks" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry
           (file+headline "Notes" "Notes")
           "* %? :NOTE:\n  %i\n  %a")))
  (org-gtd-setup)
  ;; Add custom keybindings for GTD functions
  :bind
  (:map org-mode-map
        ("C-c g i" . org-gtd-inbox)
        ("C-c g p" . org-gtd-projects)
        ("C-c g r" . org-gtd-refile)
        ("C-c g a" . org-gtd-archive)
        ("C-c g t" . org-gtd-tickler)))
