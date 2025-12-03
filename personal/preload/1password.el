(defun my/get-1pass-secret (reference)
  "Fetch a secret from 1Password using the 'op' CLI.
   REFERENCE should be a 1Password URI (e.g., op://vault/item/field)."
  ;; 1. Ensure Emacs can find the 'op' command.
  ;; If you use Homebrew on Apple Silicon, it's likely /opt/homebrew/bin.
  ;; On Intel, it's /usr/local/bin.
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path "/usr/local/bin")

  (let ((op-path (executable-find "op")))
    (if op-path
        ;; The --no-newline flag prevents trailing whitespace issues
        (string-trim (shell-command-to-string (format "%s --account RDGZPPL5VJGIRKKKZXHRNQ2ULU read \"%s\" --no-newline" op-path reference)))
      (warn "1Password CLI (op) not found in exec-path"))))
