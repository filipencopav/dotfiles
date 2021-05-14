(defun config-visit ()
  "Find init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun config-reload ()
  "Reload the configuration file (init.el)"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/init.el")))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-;") 'avy-goto-word-1)
;; Config visit/reload
(global-set-key (kbd "C-c r") 'config-reload)
(global-set-key (kbd "C-c e") 'config-visit)
;; Org agenda
(global-set-key (kbd "C-c a") 'org-agenda)

(provide 'keymappings)
