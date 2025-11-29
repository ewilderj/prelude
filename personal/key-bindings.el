;; -*- lexical-binding: t -*-
;; custom key bindings for my liking
(global-set-key (kbd "C-<end>") 'avy-goto-char-timer)
(global-set-key (kbd "C-c [") 'previous-buffer)                                   │
(global-set-key (kbd "C-c ]") 'next-buffer)                                       │


;; org mode
(global-set-key (kbd "C-c c") 'org-capture)
