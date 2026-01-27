(when (eq system-type 'darwin)

  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "FiraCode Nerd Font")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly.
  (set-face-attribute 'default nil :height 130)

  ;;    This is what we will force Tables and Code Blocks to use.
  (set-face-attribute 'fixed-pitch nil :family "FiraCode Nerd Font" :height 130)
  ;; 3. Set the Variable Pitch face (for reading text)
  (set-face-attribute 'variable-pitch nil :family "Fira Sans" :height 140
                      :weight 'regular))

(when (eq system-type 'gnu/linux)
  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "FiraCode Nerd Font")
  ;; default font size (point * 10)
  (set-face-attribute 'default nil :height 120)
  (set-face-attribute 'fixed-pitch nil :family "FiraCode Nerd Font" :height 120)
  (set-face-attribute 'variable-pitch nil :family "Fira Sans" :height 140
                      :weight 'regular))

(setq ewilderj-condense-font "Zed Mono")

;; Better emoji support - cover ranges that 'unicode alone misses
;; Must be deferred for server mode (no frame exists at preload time)
(defun ewilderj-setup-emoji-fonts (&optional _frame)
  "Set up emoji fonts. Works in both daemon and normal startup."
  (when (and (eq system-type 'darwin) (fboundp 'set-fontset-font))
    (set-fontset-font t 'emoji "Apple Color Emoji" nil 'prepend)
    (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend)
    ;; Explicit range for emoji blocks (Misc Symbols, Dingbats, Transport, etc.)
    (set-fontset-font t '(#x1F300 . #x1F9FF) "Apple Color Emoji" nil 'prepend)
    ;; Nerd Fonts grabs these - force Apple Color Emoji
    (dolist (cp '(#x1F6E0 #x1F6E1 #x1F6E2 #x1F6E3 #x1F6E4 #x1F6E5
                  #x1F6E9 #x1F6F0 #x1F6F3))
      (set-fontset-font t cp "Apple Color Emoji" nil 'prepend))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'ewilderj-setup-emoji-fonts)
  (ewilderj-setup-emoji-fonts))
