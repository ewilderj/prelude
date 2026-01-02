;; Ensure transient is loaded from straight before magit/forge
;; to avoid version conflicts with ELPA
(use-package transient
  :straight (:host github :repo "magit/transient"))

(use-package magit
  :straight t)

(use-package forge
  :after magit
  :straight t)
