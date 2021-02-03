(defun lisp-commands ()
  (setq tab-width 2)
  (paredit-mode 1))

(add-hook 'lisp-mode-hook 'lisp-commands)
(add-hook 'emacs-lisp-mode-hook 'lisp-commands)

(add-hook 'slime-repl-mode-hook (lambda ()
														 (define-key slime-repl-mode-map (kbd "C-p") 'slime-repl-previous-input)
														 (define-key slime-repl-mode-map (kbd "C-n") 'slime-repl-next-input)))

(add-hook 'prog-mode-hook (lambda ()
                            (company-mode 1)))

(add-hook 'before-save-hook (lambda ()
                              (delete-trailing-whitespace)))

(add-hook 'focus-out-hook #'garbage-collect)

(provide 'hooks)
