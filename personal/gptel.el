;;  -*- lexical-binding: t; -*-
(straight-use-package 'gptel)

(setq gptel-model 'gemini-3-pro-preview
      gptel-backend (gptel-make-gh-copilot "Copilot"))
