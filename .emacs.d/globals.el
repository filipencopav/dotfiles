(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq
 org-src-window-setup 'current-window
 ido-enable-flex-matching nil
 ido-create-new-buffer 'always
 ido-everywhere t
 ibuffer-expert t
 make-backup-files nil
 auto-save-default nil
 inferior-lisp-program "sbcl"
 inhibit-startup-message t
 column-number-mode t)

(setq-default
 indent-tabs-mode nil
 tab-width 4)


(defmacro enable-modes (&rest modes)
  `(progn
     ,@ (cl-loop for mode in modes collect `(,mode 1))))
(defmacro disable-modes (&rest modes)
  `(progn
     ,@ (cl-loop for mode in modes collect `(,mode -1))))

(enable-modes ido-mode)
(disable-modes scroll-bar-mode
               menu-bar-mode
               tool-bar-mode)

(provide 'globals)
