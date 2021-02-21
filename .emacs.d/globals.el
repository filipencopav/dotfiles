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
 column-number-mode t
 show-paren-delay 0)

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(setq-default
 indent-tabs-mode nil
 tab-width 4
 truncate-lines 1)

(defmacro enable-modes (&rest modes)
  `(progn
     ,@ (mapcar (lambda (mode) `(,mode 1)) modes)))
(defmacro disable-modes (&rest modes)
  `(progn
     ,@ (mapcar (lambda (mode) `(,mode -1)) modes)))

(enable-modes ido-mode
              show-paren-mode)
(disable-modes scroll-bar-mode
               menu-bar-mode
               tool-bar-mode)

(setq whitespace-style '(face tabs tab-mark))
(setq whitespace-display-mappings
      '((tab-mark 9 [8250 9] [92 9])))
(global-whitespace-mode)

(add-to-list 'default-frame-alist
             '(font . "xos4 Terminus 9"))

(provide 'globals)
