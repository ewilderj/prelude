;; -*- lexical-binding: t; -*-
(use-package org-todoist
  :after org
  :ensure t
  :straight (:host github
                   :repo "lillenne/org-todoist"
                   :branch "main"
                   :files ("org-todoist.el"))
  :custom
  (org-todoist-file "~/gtd/todoist.org")
  :config
  (if (eq system-type 'darwin)
      (advice-add 'browse-url-xdg-open :override #'browse-url-default-macosx-browser))
  (setopt org-todoist-api-token (getenv "TODOIST_TOKEN"))
  (setopt org-todoist-delete-remote-items t)
  (org-todoist-background-sync)
  (nconc org-capture-templates
       `(("s" "Todoist")
         ;; Capture a TODO directly to the inbox
         ("sq" "Inbox" entry (file+olp ,(org-todoist-file) "Inbox" ,org-todoist--default-section-name) "* TODO %?")
         ("si" "Inbox" entry (file+olp ,(org-todoist-file) "Inbox" ,org-todoist--default-section-name) "* TODO %? %^G %^{EFFORT}p \nSCHEDULED: %^t")
         ;; Capture to a specific project, section, and parent task, creating them if needed.
         ;; Also prompts for tags, effort, task assignment, scheduled, and deadline times
         ;; Projects are determined by projectile if possible, otherwise via an interactive prompt
         ("ss" "Select Project" entry (function org-todoist-find-project-and-section) "* TODO %^{What is the task} %^G %^{EFFORT}p %(org-todoist-assign-task) %(progn (org-schedule nil) nil) %(progn (org-deadline nil) nil)\n%?")
         ;; Capture a note to an ignored subtree
         ("sn" "Project Notes" entry (function org-todoist-project-notes) "* %?")))
  )
