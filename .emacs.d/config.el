;;; config.el --- -*- lexical-binding: t; -*-

(defvar my/config-path "~/.emacs.d/config.el")

(defun config-edit ()
  "Find config.org"
  (interactive)
  (find-file my/config-path))

(defun config-reload ()
  "Reload the configuration file"
  (interactive)
  (load-file my/config-path))

(defun my/close-help-or-xah-close-current-buffer ()
  "If currently in a *Help* buffer, bury it and delete the window. Otherwise, xah-close-current-buffer"
  (interactive)
  (if (eq major-mode 'help-mode)
      (quit-window)
    (xah-close-current-buffer)))

(leaf xah-fly-keys
  :config
  (xah-fly-keys-set-layout "qwerty")
  (define-prefix-command 'my/z-map)
  (let ((editor-keymap (make-keymap)))
    (define-key editor-keymap (kbd "r") 'config-reload)
    (define-key editor-keymap (kbd "e") 'config-edit)
    (define-key my/z-map (kbd "c") editor-keymap))
  ;; TODO: Add entire org agenda shortcuts set
  (define-key my/z-map (kbd "a") 'org-agenda)
  (define-key xah-fly-leader-key-map (kbd "z") my/z-map)
  (define-key xah-fly-leader-key-map (kbd "u") 'my/close-help-or-xah-close-current-buffer))

(eval-when-compile
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/"))
  (require 'org-element)
  (require 'org-tempo)
  (require 'org)
  (require 'ox-latex)
  (require 'ox-md)
  (require 'paren-face))

(defvar main-font nil "Font used everywhere")
(setq main-font "Fantasque Sans Mono:pixelsize=20")
(add-to-list 'default-frame-alist
             `(font . ,main-font))

(defun my/apply-emoji-font ()
    (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji" :size 13) nil 'prepend))
  (add-hook 'server-after-make-frame-hook #'my/apply-emoji-font)

(add-hook 'server-after-make-frame-hook 'my/apply-emoji-font)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(defvar my/after-theme-load-hook '() "List of functions to run after a theme has been loaded.")

(defun my/load-theme (theme &optional no-confirm no-enable)
  "Load `theme' using LOAD-THEME, afterwards running MY-AFTER-THEME-LOAD-HOOK"
  (interactive (list (intern (completing-read "Load custom theme: " (mapcar #'symbol-name (custom-available-themes)))) nil nil))
  (load-theme theme no-confirm no-enable)
  (run-hook-with-args 'my/after-theme-load-hook theme))

(defun my/custom-face-config (&optional theme)
  (when (cl-find theme '(gruvbox-dark-medium gruvbox-dark-hard gruvbox-dark-soft))
    (set-face-attribute 'default nil :foreground "#ebdbb2")
    (set-face-attribute 'mode-line nil :background "#32302F" :foreground "#FE8019")
    (set-face-attribute 'mode-line-inactive nil :background "#32302F"))
  (set-face-attribute 'mode-line nil :box nil :underline nil :overline nil)
  (set-face-attribute 'mode-line-inactive nil :box nil :underline nil :overline nil))

(add-hook 'my/after-theme-load-hook 'my/custom-face-config)

(leaf tao-theme
  :config)

(leaf solarized-theme
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-use-more-italic t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-scale-markdown-headlines t)

  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)

  (add-hook 'after-init-hook (lambda () (my/load-theme 'solarized-dark t))))

(leaf gruvbox-theme
  :config)

(leaf nord-theme
  :config)

(defun my/frame-behaviors (&optional frame)
  "Make frame- and/or terminal-local changes."
  (set-face-attribute 'default nil :font main-font)
  (set-face-attribute 'font-lock-comment-face nil :font main-font)
  (set-face-attribute 'fixed-pitch nil :font main-font)
  (set-face-attribute 'variable-pitch nil :font main-font))

(add-hook 'server-after-make-frame-hook 'my/frame-behaviors)

(defun my/length-mode-line (mode-line)
  (length (format-mode-line mode-line)))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length. Containing LEFT, and RIGHT aligned respectively."
  (let* ((window-width (window-total-width))
         (mode-line-lengths (apply #'+ (mapcar #'my/length-mode-line (list left right))))
         (available-width (- window-width mode-line-lengths))
         (format-string (format "%%%ds" available-width)))
    (append left (list (format format-string "")) right)))

(setq-default
 mode-line-format
 '((:eval
    (simple-mode-line-render
     ;; Left
     '((:eval (concat " " (if xah-fly-insert-state-p "INSERT" "NORMAL") " » "))
       "%b "
       (:eval (cond (buffer-read-only "[RO]") ((buffer-modified-p) "[+]"))))
     ;; Right
     '("line %l, col %2C")))))

(leaf eglot)
(leaf rustic
  :after (eglot)
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
  (rustic-setup-eglot))

(leaf go-mode
  :require (t project)
  :config
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (defun eglot-format-buffer-on-save ()
    (add-hook 'before-save-hook #'gofmt-before-save nil t)
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
  (add-hook 'go-mode-hook #'eglot-format-buffer-on-save))

(leaf typescript-mode
  :config
  (setq js-indent-level 2)
  (setq typescript-indent-level js-indent-level))

(leaf lua-mode)

(leaf yasnippet
  :setq
  (yas-snippet-dirs . '("~/.emacs.d/snippets"))
  :hook (prog-mode-hook . yas-minor-mode)
  :config
  (yas-reload-all))

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
        (expand-file-name "~/.emacs.d/emacs-splash.png")))

(setq initial-scratch-message
      ";; USAGE GUIDE
;; 1) Open files with File->Visit File
;; 2) Standard editor movement keys up down left right, etc. advanced commands in the menu bar
")

(defun my/generate-sbcl-image ()
  (interactive)
  (let ((location (expand-file-name "~/.emacs.d/swank/sbcl.core-for-slime")))
    (delete-file location)
    (shell-command
     (format "sbcl --eval \"(progn (mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf)) (save-lisp-and-die \\\"%s\\\"))\""
             location))))

(defun my/set-sly-mrepl-faces ()
  (let ((string-fg (face-attribute 'font-lock-string-face :foreground))
        (comment-fg (face-attribute 'font-lock-comment-face :foreground)))
    (set-face-attribute 'sly-mrepl-note-face nil :foreground comment-fg)
    (set-face-attribute 'sly-mrepl-output-face nil :foreground string-fg)))

(leaf slime
  :setq
  (inferior-lisp-program . "sbcl")
  (slime-lisp-implementations . `((sbcl ("sbcl" "--core" ,(expand-file-name "~/.emacs.d/swank/sbcl.core-for-slime")))))
  (slime-truncate-lines . nil)
  (slime-net-coding-system . 'utf-8-unix)
  :config
  (define-key slime-mode-map [remap eval-last-sexp] 'slime-eval-last-expression)
  (define-key slime-mode-map [remap eval-last-sexp] 'slime-eval-last-expression))

(leaf editorconfig
  :config
  (editorconfig-mode 1))

(leaf scss-mode
  :config
  (setq scss-compile-at-save t))

(leaf corfu
  :setq
  (corfu-auto . t)
  (corfu-quit-no-match . t)
  (corfu-auto-delay . 0.5)
  :config
  (global-corfu-mode))

(leaf vertico
  :require (t vertico-directory)
  :bind
  (:vertico-map
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char))
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :config
  (vertico-mode))

(leaf orderless
  :setq
  (completion-styles . '(basic orderless))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles partial-completion)))))

(defun my/replace-mhtml (cons)
  (if (eq (cdr cons) 'mhtml-mode)
      (cons (car cons) 'web-mode)
    cons))

(leaf web-mode
  :setq
  (web-mode-markup-indent-offset . 2)
  (web-mode-css-indent-offset . 2)
  (web-mode-code-indent-offset . 2)
  :config
  (setq auto-mode-alist (mapcar #'my/replace-mhtml auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode)))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun my/tab-insert-command () (interactive) (insert " "))

(defun my/nop () (interactive) nil)
(global-set-key (kbd "C-<tab>") 'my/tab-insert-command)
(global-set-key (kbd "M-<escape>") 'my/nop)
(define-key isearch-mode-map (kbd "M-ESC") 'my/nop)

(leaf org-contrib)

(add-to-list 'org-modules 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(defadvice org-export-output-file-name (before org-add-export-dir activate)
  "Modifies org-export to place exported files in a different directory"
  (when (not pub-dir)
    (setq pub-dir "exported-org-files")
    (when (not (file-directory-p pub-dir))
      (make-directory pub-dir))))

(setq org-html-head-include-default-style nil)
(add-to-list 'org-latex-packages-alist '("AUTO" "babel" t ("xelatex" "pdflatex")))
(add-to-list 'org-latex-packages-alist '("cache=false" "minted" t ("xelatex")))
(add-to-list 'org-latex-packages-alist '("" "titling" t ("xelatex" "pdflatex")))
(add-to-list 'org-latex-packages-alist '("" "graphicx" t ("xelatex")))
(add-to-list 'org-latex-packages-alist '("" "setspace" t ("xelatex")))
(add-to-list 'org-latex-packages-alist '("" "footmisc" t ("xelatex")))
(add-to-list 'org-latex-packages-alist '("" "fontspec" t ("xelatex")))
(add-to-list 'org-latex-packages-alist '("margin=2.5cm" "geometry" t ("xelatex")))
(add-to-list 'org-latex-packages-alist (list "" "parskip" t org-latex-compilers))

(setq org-latex-title-command nil
      org-latex-listings 'minted
      org-latex-compiler "xelatex"
      org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
      org-latex-minted-options '(("breaklines" "true") ("breakanywhere" "true") ("breaksymbolleft" "\\null"))
      org-adapt-indentation nil
      org-startup-truncated nil
      org-latex-images-centered t
      org-agenda-files (file-expand-wildcards "~/.emacs.d/org/agenda/*.org"))

(leaf org-bullets
  :custom
  ;; Default: '("◉" "○" "✸" "✿")
  ;; Second:  '("*" "●" "○" "·")
  ;; Third:   '("●" "*" "•" "·")
  ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; ►  ★ ▸
  (org-bullets-bullet-list . '("●" "*" "•" "·"))
  :hook (org-mode-hook . org-bullets-mode))

(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)

(setq org-capture-bookmark nil
      org-src-window-setup 'current-window
      org-link-descriptive nil)

;; (leaf org-roam
;;   :pre-setq (org-roam-v2-ack . t)
;;   :custom
;;   (org-roam-complete-everywhere . t)
;;   :config
;;   (setq org-roam-directory (file-truename "~/.emacs.d/org/roam/"))
;;   (define-prefix-command 'my/org-roam-commands)
;;   (define-key my/org-roam-commands (kbd "f") 'org-roam-node-find)
;;   (define-key my/org-roam-commands (kbd "i") 'org-roam-node-insert)
;;   (define-key my/org-roam-commands (kbd "j") 'org-capture)
;;   (define-key my/z-map (kbd "n") my/org-roam-commands)
;;   (org-roam-setup))

(setq org-capture-templates
      '(("f" "Fleeting note" plain (file "~/.emacs.d/org/agenda/notes.org")
         "%i\n%?" :empty-lines-before 1)
        ("t" "Org agenda TODO entry" entry (file "~/.emacs.d/org/agenda/agenda.org")
         "* TODO %?\n" :empty-lines-before 1)))
(setq org-todo-keywords
      (quote ((sequence "TODO" "DOING" "|" "DONE" "CANCELLED" "SUSPENDED"))))

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)

;; do this especially on Windows, else python output problem
(set-terminal-coding-system 'utf-8-unix)

(setq-default frame-title-format "%b - emacs")

(add-to-list 'default-frame-alist '(cursor-type . t))
(setq-default cursor-type 't)

(setq-default default-input-method "russian-computer")

(defun unfill-region (beg end)
  "Unfill the region, joining the paragraphs into a single line per paragraph."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(setq common-lisp-hyperspec-root (concat "file:///" (expand-file-name "~/.emacs.d/clhs/")))

(defun my/eww-browser-bind-advice (original-function &rest args)
  "Binds EWW as the local browser to do whatever browsing is required."
  (setq-local browse-url-browser-function 'eww-browse-url)
  (apply original-function args))
(advice-add 'hyperspec-lookup :around #'my/eww-browser-bind-advice)

(customize-set-variable 'org-file-apps
  '(("\\.pdf\\'" . "xdg-open %s")
    (auto-mode . emacs)
    (directory . default)
    ("\\.mm\\'" . default)
    ("\\.x?html?\\'" . default)))

(leaf prettier-js
  :config
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (setq
   prettier-js-args
   '("--trailing-comma" "all"
     "--arrow-parens" "avoid"
     "--single-quote")))

;; (setq font-lock-support-mode #'jit-lock-mode)
(leaf tree-sitter
  :config
  (leaf tree-sitter-langs
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

(defun my/auto-hide-compilation-window (buf str)
  (when (null (string-match ".*exited abnormally.*" str))
    (let ((win (get-buffer-window buf 'visible)))
      (when win (delete-window win)))))

(add-hook 'compilation-finish-functions 'my/auto-hide-compilation-window)

(setq help-window-select t)

(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles  nil
      inhibit-startup-screen t)

(leaf highlight-parentheses
  :config
  (setq show-paren-delay 0.2)
  (show-paren-mode 1)
  (global-highlight-parentheses-mode))

(setq require-final-newline t
      column-number-mode t
      split-width-threshold 120
      confirm-kill-process nil
      sentence-end-double-space nil)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 100
              c-default-style '((c-mode . "bsd"))
              c-basic-offset tab-width
              cperl-indent-level tab-width
              display-line-numbers-width 3)

(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(fringe-mode '(0 . 0))

(setq ibuffer-expert t
      ibuffer-show-empty-filter-groups nil
      ibuffer-saved-filter-groups
      '(("default"
         ("lisp" (or
                  (mode . lisp-mode)
                  (mode . scheme-mode)
                  (mode . emacs-lisp-mode)))
         ("org agenda"
          (filename . ".emacs.d/org/agenda/"))
         ("org" (or (mode . org-mode)
                    (name . "\\*Org Src.*\\*")))
         ("emacs" (name . "^\\*.*\\*$"))
         ("trashcan" (or
                      (name . "^\\*Compile-Log\\*$")
                      (name . "^\\*inferior-lisp\\*$")
                      (name . "^\\*slime-events\\*$"))))))

;; This switches to my default filter group and hides emacs and org agenda buffer lists by default
(add-hook 'ibuffer-mode-hook 'my/ibuffer-setup)
(defun my/ibuffer-setup ()
  (ibuffer-switch-to-saved-filter-groups "default")
  (setq ibuffer-hidden-filter-groups (list "org agenda" "emacs"))
  (ibuffer-update nil t))

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(blink-cursor-mode)

(setq custom-file "~/.emacs.d/custom.el")
(load "~/.emacs.d/custom.el")

(setq completion-auto-help 'lazy)

(setq-default auto-hscroll-mode 'current-line)

(defmacro my/add-hooks (&rest pairs)
  "Adds lambdas to the hooks of the specified modes. Takes a list of lists, the first element may be either a symbol (hook variable) or a list of symbols. The rest of the elements are "
  `(progn
     ,@(mapcan (lambda (pair)
                 (mapcar (lambda (x) `(add-hook ',x (lambda () ,@ (cdr pair))))
                         (if (listp (car pair)) (car pair) (list (car pair)))))
               pairs)))

(my/add-hooks
  ((lisp-mode-hook scheme-mode-hook emacs-lisp-mode-hook)
     (setq tab-width 2 indent-tabs-mode nil fill-column 100)
     (paren-face-mode))
  (before-save-hook
     (unless (eq major-mode 'markdown-mode)
       (delete-trailing-whitespace)))
  ((prog-mode-hook)
     (display-line-numbers-mode)
     (display-fill-column-indicator-mode))
  ((help-mode-hook sly-db-mode-hook) (visual-fill-column-mode))
  (css-mode-hook (electric-pair-local-mode)))

(global-visual-line-mode)

(leaf visual-fill-column
  :config
  (setq-default truncate-lines nil)
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (add-hook 'minibuffer-setup-hook (lambda () (visual-fill-column-mode -1)))

  (defun text-buf-wrap-setup ()
    (visual-line-mode)
    (setq visual-fill-column-center-text t))
  (add-hook 'org-mode-hook #'text-buf-wrap-setup)
  (add-hook 'text-mode-hook #'text-buf-wrap-setup)
  (add-hook 'help-mode-hook #'text-buf-wrap-setup)

  (leaf adaptive-wrap
    :config
    (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
    (add-hook 'org-mode-hook (lambda () (adaptive-wrap-prefix-mode -1))))

  (defun my/toggle-text-centering ()
    (interactive)
    (setq visual-fill-column-center-text (not visual-fill-column-center-text))))

(leaf ebnf-mode)
