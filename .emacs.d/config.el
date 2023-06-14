(defvar my/config-path "~/.emacs.d/config.el")

(defun config-edit ()
  "Find config.org"
  (interactive)
  (find-file my/config-path))

(defun config-reload ()
  "Reload the configuration file"
  (interactive)
  (load-file my/config-path))

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
  (define-key xah-fly-leader-key-map (kbd "z") my/z-map))

(eval-when-compile
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/"))
  (require 'org-element)
  (require 'org-tempo)
  (require 'org)
  (require 'ox-latex)
  (require 'ox-md)
  (require 'paren-face))

(defvar main-font nil "Font used everywhere")
(setq main-font "Fantasque Sans Mono:pixelsize=16")
(add-to-list 'default-frame-alist
             `(font . ,main-font))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(defvar my/after-theme-load-hook '() "List of functions to run after a theme has been loaded.")

(defun my/load-theme (theme &optional no-confirm no-enable)
  "Load `theme' using LOAD-THEME, afterwards running the MY-AFTER-THEME-LOAD-HOOK"
  (interactive (list (intern (completing-read "Load custom theme: " (mapcar #'symbol-name (custom-available-themes)))) nil nil))
  (load-theme theme no-confirm no-enable)
  (run-hook-with-args 'my/after-theme-load-hook theme))

(defun my/custom-face-config (&optional theme)
  (when (cl-find theme '(gruvbox-dark-medium gruvbox-dark-hard gruvbox-dark-soft))
    (set-face-attribute 'default nil :foreground "#ebdbb2")
    (set-face-attribute 'mode-line nil :background "#32302F" :foreground "#FE8019")
    (set-face-attribute 'mode-line-inactive nil :background "#32302F")))

(add-hook 'my/after-theme-load-hook 'my/custom-face-config)

(leaf tao-theme
  :config)
(leaf solarized-theme
  :config (add-hook 'after-init-hook (lambda () (my/load-theme 'solarized-dark t))))
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
  (let ((location (expand-file-name "~/.emacs.d/slynk/sbcl.core-for-slime")))
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

(leaf emojify
  :config
  (setq emojify-display-style 'unicode
        emojify-display-styles '(unicode)
        use-default-font-for-symbols nil)
  (defun my/apply-emoji-font ()
    (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji" :size 13) nil 'prepend))
  (add-hook 'server-after-make-frame-hook #'my/apply-emoji-font))

(defun my/replace-mhtml (cons)
  (when (eq (cdr cons) 'mhtml-mode)
    (setf (cdr cons) 'web-mode)))

(leaf web-mode
  :setq
  (web-mode-markup-indent-offset . 2)
  (web-mode-css-indent-offset . 2)
  :config
  (mapc #'my/replace-mhtml auto-mode-alist)
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
      org-src-window-setup 'current-window)

(leaf org-roam
  :pre-setq (org-roam-v2-ack . t)
  :custom
  (org-roam-complete-everywhere . t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n g" . org-roam-graph)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         (:org-mode-map
          ("C-M-i"  . completion-at-point)))
  :config
  (setq org-roam-directory (file-truename "~/.emacs.d/org/roam/"))
  (org-roam-setup))

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)

;; do this especially on Windows, else python output problem
(set-terminal-coding-system 'utf-8-unix)

(setq-default frame-title-format "%b - emacs")

(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(cursor-type . t))
(setq-default cursor-type 't)

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

(setq css-indent-offset 2)

(setq font-lock-support-mode #'jit-lock-mode)

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

(setq show-paren-delay 0.125)
(show-paren-mode 1)

(setq require-final-newline t
      column-number-mode t
      split-width-threshold 120
      confirm-kill-process nil)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80
              c-default-style '((c-mode . "bsd"))
              c-basic-offset tab-width
              cperl-indent-level tab-width
              display-line-numbers-width 3)

(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(fringe-mode '(0 . 0))

(setq ibuffer-expert t
      ibuffer-saved-filter-groups
      '(("default"
         ("lisp" (or
                  (mode . lisp-mode)
                  (mode . scheme-mode)
                  (mode . emacs-lisp-mode)))
         ("org" (or (mode . org-mode)
                    (name . "\\*Org Src.*\\*")))
         ("emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")))
         ("trashcan" (or
                      (name . "^\\*straight-process\\*$")
                      (name . "^\\*Compile-Log\\*$")
                      (name . "^\\*inferior-lisp\\*$")
                      (name . "^\\*slime-events\\*$"))))))

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
  (ibuffer-mode-hook (ibuffer-switch-to-saved-filter-groups "default"))
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
