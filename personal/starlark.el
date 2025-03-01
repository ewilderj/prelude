;; this script first brings in bazel with use package, then configures
;; it so bazel-starlark-mode is activated when a file with suffix
;; .star is opened it also sets up the keybinding C-c C-c to run bazel
;; build on the current file
(use-package bazel
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.star\\'" . bazel-starlark-mode))
  (add-hook 'bazel-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-c") 'bazel-build))))
