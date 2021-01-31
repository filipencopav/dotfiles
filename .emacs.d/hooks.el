(add-hook 'lisp-mode-hook (lambda ()
                            (setq tab-width 2)
                            ))

(add-hook 'prog-mode-hook (lambda ()
                            (company-mode 1)
                            ))

(add-hook 'before-save-hook (lambda ()
                              (delete-trailing-whitespace)
                              ))

(add-hook 'focus-out-hook #'garbage-collect)

(provide 'hooks)
