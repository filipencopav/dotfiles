(when (window-system)
  (set-frame-font "xos4 Terminus 9" t)
  (load-theme 'gruvbox-dark-hard t)
  (toggle-frame-maximized))

(provide 'theming)
