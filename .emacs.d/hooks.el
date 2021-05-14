(require 'c-eldoc)

(defun make-combos (pair)
                (let ((car (car pair))
                      (cdr (cdr pair)))
                  (if (listp car)
                      `(progn
                         ,@ (mapcar
                             (lambda (hook)
                               (list 'add-hook
                                     `',hook
                                     `(lambda () ,@cdr)))
                             car))
                    `(add-hook ',car (lambda () ,@cdr)))))

(defmacro add-hooks (&rest pairs)
  `(progn
     ,@ (mapcar
            #'make-combos
            pairs)))

(add-hooks
 ((lisp-mode-hook scheme-mode-hook emacs-lisp-mode-hook)
  (setq tab-width 2
        indent-tabs-mode nil)
  (paredit-mode 1)
  (highlight-parentheses-mode)
  (paren-face-mode))
 ((help-mode-hook ibuffer-mode-hook dashboard-mode-hook))
 (prog-mode-hook
  (display-line-numbers-mode)
  (company-mode)
  (yas-minor-mode))
 (before-save-hook (delete-trailing-whitespace))
 (focus-out-hook (garbage-collect))
 (org-mode-hook (auto-fill-mode))
 ((text-mode-hook org-mode-hook
                  prog-mode-hook)
  (display-fill-column-indicator-mode))
 ((c-mode-hook c++-mode-hook) (c-turn-on-eldoc-mode)))

(provide 'hooks)
