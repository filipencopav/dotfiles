;; Fast startup
(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/globals.el")
(load "~/.emacs.d/hooks.el")
(load "~/.emacs.d/org-config.el")
(load "~/.emacs.d/theming.el")
(load "~/.emacs.d/keymappings.el")
