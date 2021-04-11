(require 'c-eldoc)

(defun disable-fill-column-indicator-mode ()
  (display-fill-column-indicator-mode -1))

(defmacro add-hooks (&rest pairs)
  `(progn
     ,@(mapcar
        (lambda (pair)
          (cond
           ((listp (car pair))
            `(progn
               ,@(mapcar (lambda (hook)
                           (list 'add-hook `',hook (cadr pair)))
                         (car pair))))
           (t `(add-hook ',(car pair) ,(cadr pair)))))
        pairs)))

(add-hooks
 ((lisp-mode-hook scheme-mode-hook emacs-lisp-mode-hook)
  (lambda ()
    (setq tab-width 2
          indent-tabs-mode nil)
    (paredit-mode 1)
    (prettify-symbols-mode 1)
    (highlight-parentheses-mode)
    (paren-face-mode)))
 ((help-mode-hook ibuffer-mode-hook dashboard-mode-hook)
  (lambda ()
    (display-fill-column-indicator-mode -1)))

 (prog-mode-hook #'company-mode)
 (before-save-hook #'delete-trailing-whitespace)
 (focus-out-hook #'garbage-collect)
 (org-mode-hook #'auto-fill-mode)
 ((c-mode-hook c++-mode-hook) #'c-turn-on-eldoc-mode))

(provide 'hooks)
