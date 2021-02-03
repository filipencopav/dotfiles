(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq org-src-window-setup 'current-window
 ido-enable-flex-matching nil
 ido-create-new-buffer 'always
 ido-everywhere t
 ibuffer-expert t
 make-backup-files nil
 auto-save-default nil
 inferior-lisp-program "sbcl"
 inhibit-startup-message t)

(setq-default
 indent-tabs-mode nil
 tab-width 4)

(ido-mode 1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode  -1)

(provide 'globals)
