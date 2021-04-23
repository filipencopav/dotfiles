(defun config-visit ()
  "Find init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  "Reload the configuration file (init.el)"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c r") 'config-reload)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-;") 'avy-goto-char-1)

(provide 'keymappings)
