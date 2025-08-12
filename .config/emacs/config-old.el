;;; config.el --- -*- lexical-binding: t; -*-

;; (defvar main-font nil "Font used everywhere")
;; (setq main-font "FantasqueSansM Nerd Font:pixelsize=16:antialias=true")
;; (add-to-list 'default-frame-alist
;;              `(font . ,main-font))

(leaf rustic
  :after eglot
  :mode ("\\.rs\\'" . rustic-mode)
  :bind (:rustic-mode-map
         ("M-j" . eglot-imenu)
         ("M-?" . xref-find-references)
         ("C-c C-c r" . eglot-rename)
         ("C-c C-c q" . eglot-reconnect)
         ("C-c C-c Q" . eglot-shutdown)
         ("C-c C-c o" . eglot-code-action-organize-imports)
         ("C-c C-c b" . rustic-cargo-build)
         ("C-c C-c c" . rustic-cargo-check))
  :init
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-lsp-client 'eglot)
  :config
  (setq rustic-analyzer-command '("rustup" "run" "nightly" "rust-analyzer"))
  (setq rustic-format-on-save t)
  (rustic-setup-eglot)
  (defun my/rustic-functions ()
    (setq-local syntax-propertize-function nil))
  (add-hook 'rustic-mode-hook 'my/rustic-functions))

(defun my/choose-initial-buffer ()
  (if (get-buffer-window "*dashboard*" 'visible)
      t
    (get-buffer-create "*dashboard*")))

(leaf dashboard
  :setq
  ;; (initial-buffer-choice       . 'my/choose-initial-buffer)
  (dashboard-banner-logo-title . "O U R macs")
  (dashboard-center-content    . t)
  (dashboard-page-separator    . "\n")

  (dashboard-items . '((bookmarks . 4)
                       (agenda . 4)))
  :config
  ;; (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner
        (expand-file-name "emacs-splash.png" *emacs-config-location*)))

(setq initial-scratch-message "# Org mode scratch buf\n\n"
      initial-major-mode 'org-mode)



;; (leaf slime
;;   :setq
;;   (inferior-lisp-program . "sbcl")
;;   (slime-lisp-implementations . `((sbcl ("sbcl" "--core" ,(expand-file-name "swank/sbcl.core-for-slime" *emacs-config-location*)))))
;;   (slime-truncate-lines . nil)
;;   (slime-net-coding-system . 'utf-8-unix)
;;   :config
;;   (define-key slime-mode-map [remap eval-last-sexp] 'slime-eval-last-expression)
;;   (define-key slime-mode-map [remap eval-last-sexp] 'slime-eval-last-expression))



(setq common-lisp-hyperspec-root (concat "file:///" (expand-file-name "clhs/" *emacs-config-location*)))

(defun my/eww-browser-bind-advice (original-function &rest args)
  "Binds EWW as the local browser to do whatever browsing is required."
  (setq-local browse-url-browser-function 'eww-browse-url)
  (apply original-function args))
(advice-add 'hyperspec-lookup :around #'my/eww-browser-bind-advice)





(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
;; (fringe-mode 1)


(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(blink-cursor-mode)

(setq custom-file (expand-file-name "custom.el" *emacs-config-location*))
(load custom-file)

(setq completion-auto-help 'lazy)

(setq-default auto-hscroll-mode 'current-line)



(global-visual-line-mode)
(global-auto-revert-mode t)
