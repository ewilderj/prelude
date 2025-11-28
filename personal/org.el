;; -*- lexical-binding: t; -*-

;; see straight.el for the main loading of org

;; BEGIN org-mode customization

;; here's every customization of vanilla org
;; config specific to org extensions can be found in their
;; use-package declarations

(setq org-confirm-elisp-link-function nil)

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/projects.org"
                         "~/gtd/tickler.org"
                         ))

;; save org buffers when we idle
(setq ewj/org-timer (run-with-idle-timer 300 t 'org-save-all-org-buffers))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("m" "Meeting" entry (file+headline "~/gtd/inbox.org" "Tasks")
                               "* MEETING with %? :meeting:\n%U")
                              ("n" "Note" entry (file+headline "~/gtd/notes.org" "Notes")
                               "* %? :note:\n%U\n")
                              ("j" "Journal" entry (file+datetree "~/gtd/journal.org")
                               "* %?\n%U\n")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)" "PHONE" "MEETING"))))

(add-hook 'org-capture-mode-hook #'auto-fill-mode)
(setq org-ellipsis " ⤵")
(setq org-refile-targets '(("~/gtd/projects.org" :maxlevel . 3)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/notes.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))

;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling
;; END org-mode customization


(use-package ox-pandoc
  :after org
  :ensure t)

(use-package org-bullets
  :after org
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-projectile
  :after org
  :ensure t
  :config
  (setq org-projectile-projects-file "~/gtd/projectile-todo.org")
  (org-projectile-single-file)
  (push (org-projectile-project-todo-entry) org-capture-templates)
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package org-super-agenda
  :after org
  :ensure t
  :config
  (add-hook 'org-agenda-mode-hook #'org-super-agenda-mode)
  (setq org-agenda-custom-commands
        '(
          ("n" todo "NEXT")
          ("w" todo "WAITING")
          ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT")))
          ("b" "Agenda + TODO" ((agenda) (todo)))
          ("u" "Super view" (
                             (agenda "" ((org-agenda-span 1)
                                         (org-super-agenda-groups
                                          '((:name "Today"
                                                   :time-grid t)
                                            (:habit t)
                                            (:log t)))))

                             (todo "" ((org-agenda-overriding-header "Next Actions")
                                       (org-super-agenda-groups
                                        '((:name none  ; Disable super group header
                                                 :todo ("NEXT"))
                                          (:discard (:anything t))
                                          ))))
                             ) ; end Super view
           ))
        ) ; end org-agenda-custom-commands
  )
