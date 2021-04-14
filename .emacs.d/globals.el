(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq org-src-window-setup 'current-window
      ido-enable-flex-matching nil
      ido-create-new-buffer 'always
      ido-everywhere t
      ibuffer-expert t
      make-backup-files nil
      auto-save-default nil
      inferior-lisp-program "sbcl"
      inhibit-startup-message t
      column-number-mode t
      show-paren-delay 0
      c-eldoc-buffer-regenerate-time 20)

(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines 1
              fill-column 80
              c-default-style '((c-mode . "bsd"))
              c-basic-offset tab-width
              cperl-indent-level tab-width)

(ido-mode 1)
(show-paren-mode 1)
(global-display-line-numbers-mode)
(global-display-fill-column-indicator-mode)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; whitespace
(setq whitespace-style '(face tabs tab-mark)
      whitespace-display-mappings '((tab-mark 9 [8250 9] [92 9])))
(global-whitespace-mode)

(add-to-list 'default-frame-alist
             '(font . "xos4 Terminus 9"))

(provide 'globals)
