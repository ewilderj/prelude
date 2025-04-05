;; -*- lexical-binding: t; -*-

(setq org-gtd-update-ack "3.0.0")

(use-package org-gtd
  :ensure t
  :after org
  :custom
    (org-gtd-directory "~/org/gtd")
    (org-edna-use-inheritance t)
    (org-gtd-clarify-show-horizons t)
    (org-gtd-areas-of-focus '("Manager" "Copilot" "SFI" "Career" "EPD Ops"))
  :config
  (org-edna-mode)
  (define-key org-gtd-clarify-map (kbd "C-c c") #'org-gtd-organize)
  (define-key org-gtd-process-map (kbd "C-c c") #'org-gtd-choose)
  ;; Add custom keybindings for GTD functions
  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-show-stuck-projects)
   :map org-gtd-process-map
   ("C-c c" . org-gtd-choose)))

(defvar my-gtd-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'org-gtd-capture)
    (define-key map "e" 'org-gtd-engage)
    (define-key map "p" 'org-gtd-process-inbox)
    (define-key map "n" 'org-gtd-show-all-next)
    (define-key map "s" 'org-gtd-show-stuck-projects)
    map)

  "My GTD keymap.")

(keymap-global-set "s-g" my-gtd-map)
