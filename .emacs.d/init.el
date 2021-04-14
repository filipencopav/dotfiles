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

(load "~/.emacs.d/meghanada-conf.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(gruvbox-theme slime paredit meghanada highlight-parentheses geiser dashboard avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
