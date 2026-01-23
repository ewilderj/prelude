;; -*- lexical-binding: t; -*-

;; see straight.el for the main loading of org

;; BEGIN org-mode customization

;; here's every customization of vanilla org
;; config specific to org extensions can be found in their
;; use-package declarations

(setq org-confirm-elisp-link-function nil)

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/work.org"
                         "~/gtd/projects.org"
                         "~/gtd/tickler.org"
                         "~/gtd/todoist.org"
                         "~/gtd/outlook.org"
                         ))

;; aesthetics, from https://github.com/minad/org-modern
(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-tags-column 0
 org-ellipsis "…")

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
              (sequence "WAITING(w)" "HOLD(h)" "PHONE" "MEETING" "|" "CANCELED(c)"))))

(add-hook 'org-capture-mode-hook #'auto-fill-mode)

(setq org-refile-targets '(("~/gtd/projects.org" :maxlevel . 3)
                           ("~/gtd/todoist.org" :maxlevel . 4)
                           ("~/gtd/work.org" :maxlevel . 3)
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
                                          '((:habit t)
                                            (:log t)
                                            (:name "Schedule"
                                                   :time-grid t)
                                            (:name "Events"
                                                   :anything t)))))

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

(use-package adaptive-wrap
  :straight t
  :after org
  :ensure t
  :hook ((visual-line-mode . adaptive-wrap-prefix-mode)
         (org-mode . visual-line-mode)
         (org-mode . org-indent-mode)))

(use-package org-modern
  :after org
  :ensure t
;;  :custom
;;  (org-modern-table nil)  ; Disable table prettification - incompatible with mixed-pitch
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(use-package org-tidy
  :after org
 :ensure t
 :hook
 (org-mode . org-tidy-mode)
 :config
 (define-key org-mode-map (kbd "C-c C-x t") #'org-tidy-toggle)
 )

(use-package mixed-pitch
  :after org
  :ensure t
  :hook
  (text-mode . ewj/maybe-mixed-pitch)
  :init
  (defun ewj/maybe-mixed-pitch ()
    "Enable mixed-pitch-mode unless in yaml or similar modes."
    (unless (derived-mode-p 'yaml-mode 'yaml-ts-mode)
      (mixed-pitch-mode 1)))
  ;; :custom
  ;; (mixed-pitch-face 'ewj/reading-face)
  ;; (mixed-pitch-set-height t)
  :config
  (setq mixed-pitch-fixed-pitch-faces
        (append mixed-pitch-fixed-pitch-faces
                '(org-table
                  org-modern-label
                  org-modern-date
                  org-modern-time
                  org-special-keyword       ; For TODO, DONE etc
                  org-modern-symbol         ; Vital for org-modern table borders
                  org-modern-tag)))
  )


(defun ewj/org-fixed-pitch-tables ()
  "Apply fixed-pitch face to entire org tables, including spaces."
  (font-lock-add-keywords
   nil
   '(("^[ \t]*\\(|.*|\\)[ \t]*$"
      (0 'fixed-pitch append)))
   'append))

(add-hook 'org-mode-hook #'ewj/org-fixed-pitch-tables)

(defun ewj/find-meeting-context ()
  "Find the current or next meeting from outlook.org and return a plist with details."
  (let ((meetings '())
        (now (current-time))
        (today (format-time-string "%Y-%m-%d")))
    (with-current-buffer (find-file-noselect "~/gtd/outlook.org")
      (save-excursion
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (let* ((element (org-element-at-point))
                  (title (org-element-property :title element))
                  (ts-str (org-entry-get nil "TIMESTAMP")))
             (when (and ts-str (string-match today ts-str))
               (let ((ts (org-parse-time-string ts-str)))
                 (let ((start-time (apply #'encode-time ts))
                       (end-time nil))
                   (when (string-match "--\\([0-9]+:[0-9]+\\)" ts-str)
                     (let ((end-str (match-string 1 ts-str)))
                       (setq end-time (apply #'encode-time
                                             (append (list 0
                                                           (string-to-number (substring end-str 3 5))
                                                           (string-to-number (substring end-str 0 2)))
                                                     (nthcdr 3 ts))))))
                   
                   (unless end-time
                     (if (equal (nth 2 ts) 0) 
                         (setq end-time (time-add start-time (seconds-to-time 86399)))
                       (setq end-time (time-add start-time (seconds-to-time 3600)))))

                   ;; Get content (description) and properties
                   ;; org-get-entry returns content including properties drawer.
                   ;; We might want to keep the drawer.
                   (let ((content (org-get-entry)) 
                         (props (org-entry-properties)))
                     (push (list :title title
                                 :start start-time
                                 :end end-time
                                 :content content
                                 :props props)
                           meetings)))))))
         t))) 
    
    (setq meetings (sort meetings (lambda (a b) (time-less-p (plist-get a :start) (plist-get b :start)))))
    
    (let ((current-meeting nil)
          (next-meeting nil))
      (dolist (m meetings)
        (let ((start (plist-get m :start))
              (end (plist-get m :end)))
          (cond
           ((and (or (time-less-p start now) (equal start now))
                 (time-less-p now end))
            (setq current-meeting m))
           ((time-less-p now start)
            (unless next-meeting (setq next-meeting m))))))
      
      (or current-meeting next-meeting))))

(defun ewj/format-meeting-capture ()
  "Format the meeting context for the capture template."
  (let ((meeting (ewj/find-meeting-context)))
    (if meeting
        (let ((title (plist-get meeting :title))
              (content (plist-get meeting :content)))
          (format "* MEETING Notes: %s :meeting:\n%s" 
                  title 
                  (or content "")))
      (format "* MEETING Notes: Quick Meeting :meeting:\n%s\n" 
              (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))))))

(defun ewj/capture-meeting-notes ()
  "Capture notes for the current or next meeting."
  (interactive)
  (org-capture nil "M"))

;; personal strategy capture stuff
(setq org-capture-templates
      (append org-capture-templates
              '(("M" "Meeting Notes" entry
                 (file+headline "~/gtd/inbox.org" "Tasks")
                 "%(ewj/format-meeting-capture)
%?")
                ("s" "Daily Strategy Card" entry
                 (file+olp+datetree "~/gtd/journal.org")
                 "* Daily Strategic Review: The Vital Three :strategy:
%U
** The Strategic Grip
\"Review these pillars before opening Slack or Email.\"

*** Pillar 1: Technical Sovereignty
- Focus: AI-integrated TPM workflows + Personal Projects
- [ ] Next Stop Sign: %^{Next Step (Tech)}

*** Pillar 2: Financial Resilience
- Focus: Coaching & Side Hustle Ideation
- [ ] Next Stop Sign: %^{Next Step (Finance)}

*** Pillar 3: Institutional Advancement
- Focus: Exec Connections & Amador Salons
- [ ] Next Stop Sign: %^{Next Step (Inst)}

** The Maintenance Floor
\"Clear the friction to protect the focus.\"
- [ ] Next Stop Sign (Admin/House): %^{Next Step (Floor)}

** Easy Mode Checklist
- [ ] Movement: Is the transition walk scheduled?
- [ ] Gap Check: Am I avoiding a \"Mountain\"? Break it into \"Paragraphs.\"
- [ ] Isolation Veto: Have I signaled a +1 friend (Julie, Scott, Dave, John, Gavin)?

** Evening Shutdown
- [ ] Shutdown Boundary: Laptop closed after transition walk.
- [ ] Sleep Anchor: Target 10:30 PM (No negotiations)."
                 :immediate-finish nil :jump-to-captured t))))
