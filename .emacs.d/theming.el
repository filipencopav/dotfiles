(when (window-system)
  (toggle-frame-maximized))

(add-hook 'after-init-hook
          (lambda ()
            (load-theme 'gruvbox-dark-hard t)))

(provide 'theming)
