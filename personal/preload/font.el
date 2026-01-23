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
(when (and (eq system-type 'darwin) (fboundp 'set-fontset-font))
  (set-fontset-font t 'emoji "Apple Color Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend)
  ;; Explicit range for emoji blocks (Misc Symbols, Dingbats, Transport, etc.)
  (set-fontset-font t '(#x1F300 . #x1F9FF) "Apple Color Emoji" nil 'prepend))
