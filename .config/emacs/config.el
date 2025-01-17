;;; config.el --- -*- lexical-binding: t; -*-

(defmacro my/add-hooks (&rest pairs)
  "Adds lambdas to the hooks of the specified modes. Takes a list of lists, the first element may be either a symbol (hook variable) or a list of symbols. The rest of the elements are "
  `(progn
     ,@(mapcan (lambda (pair)
                 (mapcar (lambda (x) `(add-hook ',x (lambda () ,@ (cdr pair))))
                         (if (listp (car pair)) (car pair) (list (car pair)))))
               pairs)))

(defun config-edit ()
  "Find config.org"
  (interactive)
  (find-file *config.el-location*))

(defun config-reload ()
  "Reload the configuration file"
  (interactive)
  (load-file *config.el-location*))

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
    (define-key editor-keymap (kbd "c") 'org-capture)
    (define-key my/z-map (kbd "c") editor-keymap))
  ;; TODO: Add entire org agenda shortcuts set
  (define-key my/z-map (kbd "a") 'org-agenda)
  (define-key xah-fly-leader-key-map (kbd "z") my/z-map)
  (define-key xah-fly-leader-key-map (kbd "u") 'my/close-help-or-xah-close-current-buffer))

(xah-fly-keys)

(eval-when-compile
  (add-to-list 'load-path (expand-file-name "elisp" *emacs-config-location*))
  (require 'org-element)
  (require 'org-tempo)
  (require 'org)
  (require 'ox-latex)
  (require 'ox-md)
  (require 'window-layout))

(leaf org
  :custom
  (org-babel-load-languages . '((emacs-lisp . t) (lisp . t)))
  (org-todo-keywords . '((sequence "TODO" "|" "DONE" "CANCELLED" "SUSPENDED")))
  ((org-confirm-babel-evaluate
    org-capture-bookmark
    org-link-descriptive
    org-html-head-include-default-style
    org-latex-title-command
    org-adapt-indentation
    org-startup-truncated)
   . nil)
  ((org-latex-images-centered) . t)
  (org-src-window-setup . 'current-window)
  (org-latex-listings . 'listings)
  (org-latex-compiler . "tectonic")
  (org-latex-pdf-process . '("tectonic --outdir %o %f"))
  (org-latex-minted-options . '(("breaklines" "true")
                                ("breakanywhere" "true")
                                ("breaksymbolleft" "\\null")))
  (org-agenda-files . `(,@(file-expand-wildcards
                           (expand-file-name
                            "org/agenda/*.org"
                            *emacs-config-location*))))
  (org-capture-templates
   .
   `(("f" "Fleeting note" plain
      (file ,(expand-file-name "org/agenda/notes.org" *emacs-config-location*))
      "%i\n%?" :empty-lines-before 1)

     ("t" "Org agenda TODO entry" entry
      (file ,(expand-file-name "org/agenda/agenda.org" *emacs-config-location*))
      "* TODO %?\n" :empty-lines-before 1)

     ("k" "Organizational TODO entry" entry
      (file ,(expand-file-name "org/agenda/komm.org" *emacs-config-location*))
      "* TODO %?\n" :empty-lines-before 1)

     ("p" "New priority" entry
      (file ,(expand-file-name
              "org/agenda/priorities.org"
              *emacs-config-location*))
      "* TODO %?\n" :empty-lines-before 0)))

  :config
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  (defadvice org-export-output-file-name (before org-add-export-dir activate)
    "Modifies org-export to place exported files in a different directory"
    (when (not pub-dir)
      (setq pub-dir "exported-org-files")
      (when (not (file-directory-p pub-dir))
        (make-directory pub-dir))))

  (setq org-latex-compilers '("tectonic" "pdflatex" "xelatex" "lualatex"))
  (dolist (pkg (list '("AUTO" "babel" t ("tectonic" "xelatex" "pdflatex"))
                     '("cache=false" "minted" t ("xelatex"))
                     '("" "titling" t ("tectonic" "xelatex" "pdflatex"))
                     '("" "graphicx" t ("tectonic" "xelatex"))
                     '("" "setspace" t ("tectonic" "xelatex"))
                     '("" "footmisc" t ("tectonic" "xelatex"))
                     '("" "fontspec" t ("tectonic" "xelatex" "lualatex"))
                     '("margin=2.5cm" "geometry" t ("tectonic" "xelatex"))
                     `("" "parskip" t ,org-latex-compilers)
                     `("" "listings" t ,org-latex-compilers)))
    (add-to-list 'org-latex-packages-alist pkg))

  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers))

(leaf org-contrib
  :after (org))

(leaf org-bullets
  :after (org)
  :custom
  ;; Default: '("◉" "○" "✸" "✿")
  ;; Second:  '("*" "●" "○" "·")
  ;; Third:   '("●" "*" "•" "·")
  ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; ►  ★ ▸
  (org-bullets-bullet-list . '("●" "*" "•" "·"))
  :hook (org-mode-hook . org-bullets-mode))

(leaf org-roam
  :pre-setq (org-roam-v2-ack . t)
  :custom
  (org-roam-complete-everywhere . t)
  :config
  (setq org-roam-directory (-> "org/roam/"
                               (expand-file-name *emacs-config-location*)
                               (file-truename)))
  (define-prefix-command 'my/org-roam-commands)
  (define-key my/org-roam-commands (kbd "f") 'org-roam-node-find)
  (define-key my/org-roam-commands (kbd "i") 'org-roam-node-insert)
  (define-key my/org-roam-commands (kbd "j") 'org-capture)
  (define-key my/z-map (kbd "n") my/org-roam-commands)
  (org-roam-setup))

(leaf org-roam-ui
  :require (org-roam t)
  :after org-roam)

;; https://github.com/kiwanami/emacs-window-layout
;; (setq wm
;;       (wlf:layout
;;        '(- (:upper-size-ratio 0.8)
;;            (| (:left-size-ratio 0.6)
;;               code
;;               (- (:upper-max-size 15)
;;                  repl
;;                  help))
;;            output)
;;        '((:name output :buffer "output buffer")
;;          (:name code :buffer "code buffer")
;;          (:name repl :buffer "repl buffer")
;;          (:name help :buffer "*Help*"))))

(defvar main-font nil "Font used everywhere")
(setq main-font "FantasqueSansM Nerd Font:pixelsize=14:antialias=true")
(add-to-list 'default-frame-alist
             `(font . ,main-font))

(defun my/apply-emoji-font ()
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji" :size 13) nil 'prepend))
(add-hook 'server-after-make-frame-hook #'my/apply-emoji-font)

(add-hook 'server-after-make-frame-hook 'my/apply-emoji-font)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" *emacs-config-location*))

(defvar my/after-theme-load-hook (list)
  "List of functions to run after a theme has been loaded.")

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
  (setq solarized-height-plus-4 1.0))

(leaf gruvbox-theme
  :config)

(leaf nord-theme
  :config)

(add-hook 'after-init-hook (lambda () (my/load-theme 'modus-operandi t)))

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
     '("line %3l, col %3c")))))

(leaf eldoc-box
  :init
  (defun my/eldoc-box-scroll-up ()
    "Scroll up in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-down 3))))
  (defun my/eldoc-box-scroll-down ()
    "Scroll down in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-up 3))))
  :after eglot
  :custom
  (eldoc-box-max-pixel-height . 200)
  :bind (:eglot-mode-map
         ("M-i" . my/eldoc-box-scroll-up)
         ("M-k" . my/eldoc-box-scroll-down))
  :config
  (defun my/eldoc-hooks ()
    (interactive)
    (eldoc-box-hover-mode))
  (add-hook 'eldoc-mode-hook 'my/eldoc-hooks))

(leaf eglot
  (with-eval-after-load 'eglot
    (setf (alist-get '(elixir-mode elixir-ts-mode heex-ts-mode)
                     eglot-server-programs
                     nil nil #'equal)
          (if (and (fboundp 'w32-shell-dos-semantics)
                   (w32-shell-dos-semantics))
              '("language_server.bat")
            (eglot-alternatives
             '("language_server.sh" "start_lexical.sh"))))))

(leaf elixir-ts-mode)

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

(leaf scala-ts-mode
  :after (eglot treesit-auto)
  :interpreter ("scala" . scala-ts-mode)
  :hook (scala-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(scala-ts-mode "metals-emacs"))
  (setq-default eglot-workspace-configuration
                (plist-put eglot-workspace-configuration
                           :metals
                           '(:superMethodLensesEnabled t :showInferredType t))))

(leaf sbt-mode
  :commands (sbt-start sbt-command)
  :config
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(leaf reformatter)
(leaf go-ts-mode
  :after (reformatter)
  :require (t project reformatter)
  :hook
  (go-ts-mode-hook . eglot-ensure)
  (go-ts-mode-hook . gofmt-on-save-mode)
  (go-ts-mode-hook . goimports-on-save-mode)
  (go-ts-mode-hook . subword-mode)
  :init
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  (add-to-list 'major-mode-remap-alist '(go-dot-mod-mode . go-mod-ts-mode))
  :custom
  (go-ts-mode-indent-offset . 4)
  :config
  (reformatter-define gofmt
    :program "gofmt"
    :lighter "GoFmt"
    :group 'go-format)
  (reformatter-define goimports
    :program "goimports"
    :lighter "GoImp"
    :group 'go-format)

  (when-let (cache-dir (getenv "XDG_CACHE_HOME"))
    (add-to-list 'eglot-server-programs
                 `((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode)
                   "gopls" "-logfile" ,(expand-file-name "gopls/emacs.log" cache-dir)
                   "-rpc.trace")))

  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))
  (add-hook 'project-find-functions #'project-find-go-module))

(leaf protobuf-ts-mode
  :mode ("\\.proto\\'" . protobuf-ts-mode))

(leaf dockerfile-ts-mode)

(leaf erlang
  :require '(t erlang-start)
  :after (eglot reformatter)
  :hook
  (erlang-mode-hook . eglot-ensure)
  :mode ("/?rebar\\.config\\'" . erlang-mode)
  :setq (erlang-electric-commands . '(erlang-electric-gt))
  :config
  (add-to-list 'eglot-server-programs
               '(erlang-mode "elp" "server")))

(leaf typescript-ts-mode
  :after eglot
  :config
  (setq js-indent-level 2)
  (setq typescript-indent-level js-indent-level)
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode typescript-mode) "deno" "lsp"))
  (add-to-list 'typescript-ts-mode-hook #'eglot-ensure))

(leaf lua-mode)

(leaf ada-mode
  :config
  (add-hook 'ada-mode-hook (lambda () (indent-tabs-mode -1)))
  (defun project-find-gpr-build (dir)
    (when-let ((root (locate-dominating-file dir "build.gpr")))
      (cons 'gpr-build root)))

  (cl-defmethod project-root ((project (head gpr-build)))
    (cdr project))
  (add-hook 'project-find-functions #'project-find-gpr-build))

(leaf typst-ts-mode
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode.git")
  :after tree-sitter)

(leaf yasnippet
  :hook (prog-mode-hook . yas-minor-mode)
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" *emacs-config-location*)))
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
        (expand-file-name "emacs-splash.png" *emacs-config-location*)))

(setq initial-scratch-message "# Org mode scratch buf\n\n"
      initial-major-mode 'org-mode)

(defvar *sly-image-location*
  (expand-file-name "lisp/sbcl.core-for-sly" *emacs-config-location*))

(defun my/generate-sly-image ()
  (interactive)
  (shell-command
   (format
   "sbcl \\
--eval \"(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))\" \\
--load %s \\
--eval '(slynk-loader:dump-image \"%s\")'"
   (expand-file-name "sly/slynk/slynk-loader.lisp" *emacs-config-location*)
   *sly-image-location*)))

(defun my/set-sly-mrepl-faces ()
  (let ((string-fg (face-attribute 'font-lock-string-face :foreground))
        (comment-fg (face-attribute 'font-lock-comment-face :foreground)))
    (set-face-attribute 'sly-mrepl-note-face nil :foreground comment-fg)
    (set-face-attribute 'sly-mrepl-output-face nil :foreground string-fg)))

(leaf sly
  :after org
  :custom
  (org-babel-lisp-eval-fn . #'sly-eval)
  (inferior-lisp-program . "sbcl")
  (sly-truncate-lines . nil)
  (sly-net-coding-system . 'utf-8-unix)
  :setq
  (sly-lisp-implementations
   .
   `((sbcl ("sbcl" "--core" ,*sly-image-location*)
           :init (lambda (port-file _)
                   (format "(slynk:start-server %S)\n"
                           port-file)))))
  :config
  (with-eval-after-load 'sly-mrepl
    (define-key sly-mrepl-mode-map [remap eval-last-sexp] 'sly-eval-last-expression))
  (define-key sly-mode-map [remap eval-last-sexp] 'sly-eval-last-expression))

;; (leaf slime
;;   :setq
;;   (inferior-lisp-program . "sbcl")
;;   (slime-lisp-implementations . `((sbcl ("sbcl" "--core" ,(expand-file-name "swank/sbcl.core-for-slime" *emacs-config-location*)))))
;;   (slime-truncate-lines . nil)
;;   (slime-net-coding-system . 'utf-8-unix)
;;   :config
;;   (define-key slime-mode-map [remap eval-last-sexp] 'slime-eval-last-expression)
;;   (define-key slime-mode-map [remap eval-last-sexp] 'slime-eval-last-expression))

(leaf clojure-ts-mode
  :config
  (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode)))

(leaf cider
  :require (t cider-eval)
  :config
  (define-key cider-mode-map [remap eval-last-sexp] 'cider-eval-last-sexp)
  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)
  (setq clojure-indent-style 'always-indent)
  (setq cider-repl-display-output-before-window-boundaries t)
  (setq cider-show-error-buffer t)
  ;; do not indent single ; character
  (add-hook 'clojure-mode-hook (lambda () (setq-local comment-column 0)))

  (setq cider-clojure-cli-aliases ":dev"))

(leaf editorconfig
  :config
  (editorconfig-mode 1))

(leaf corfu
  :custom
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
  (if (member (cdr cons) '(mhtml-mode))
      (cons (car cons) 'web-mode)
    cons))

(leaf web-mode
  :config
  (setq auto-mode-alist (mapcar #'my/replace-mhtml auto-mode-alist))
  (setq web-mode-auto-close-style 2)

  (setq web-mode-markup-indent-offset  2)
  (setq web-mode-css-indent-offset     2)

  (setq web-mode-enable-auto-pairing   t)
  (setq web-mode-enable-auto-closing   t)
  (setq web-mode-enable-auto-indentation t)

  (setq web-mode-markup-indent-offset  2)
  (setq web-mode-css-indent-offset     2)
  (setq web-mode-code-indent-offset    2)
  (setq web-mode-enable-current-element-highlight t)

  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

  (defun my/web-mode-hooks ()
    "Hooks for Web mode."
    )

  (defun my/scss-compile ()
    (interactive)
    (compile
     (concat
      "sassc " ;; FIXME: should I put it into a variable instead?
      "--sourcemap=auto "
      "'" buffer-file-name "'"
      " "
      "'" (replace-regexp-in-string "\\.scss\\'" ".css" buffer-file-name) "'")))

  (add-hook 'web-mode-hook 'my/web-mode-hooks))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun my/tab-insert-command () (interactive) (insert " "))

(defun my/nop () (interactive) nil)
(global-set-key (kbd "C-<tab>") 'my/tab-insert-command)
(global-set-key (kbd "M-<escape>") 'my/nop)
(define-key isearch-mode-map (kbd "M-ESC") 'my/nop)

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

(setq common-lisp-hyperspec-root (concat "file:///" (expand-file-name "clhs/" *emacs-config-location*)))

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
(leaf treesit-auto
  :custom
  '(treesit-auto-install . t)
  :config
  (global-treesit-auto-mode))

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

(leaf ispell
  :custom
  (ispell-program-name . "aspell"))

(leaf paren-face
  :config
  (my/add-hooks
   ((lisp-mode-hook scheme-mode-hook emacs-lisp-mode-hook clojure-mode-hook)
    (paren-face-mode))))

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
(fringe-mode 1)

(setq ibuffer-expert t
      ibuffer-show-empty-filter-groups nil
      ibuffer-saved-filter-groups
      `(("default"
         ("lisp" (or
                  (mode . lisp-mode)
                  (mode . scheme-mode)
                  (mode . emacs-lisp-mode)))
         ("org agenda"
          (filename . ,(expand-file-name "org/agenda/" *emacs-config-location*)))
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
  (ibuffer-update nil t)
  (setq-local truncate-partial-width-windows nil)
  (visual-fill-column-mode -1)
  (visual-line-mode -1)
  (toggle-truncate-lines 1)
  )

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(blink-cursor-mode)

(setq custom-file (expand-file-name "custom.el" *emacs-config-location*))
(load custom-file)

(setq completion-auto-help 'lazy)

(setq-default auto-hscroll-mode 'current-line)

(my/add-hooks
  ((lisp-mode-hook scheme-mode-hook emacs-lisp-mode-hook clojure-mode-hook)
   (setq indent-tabs-mode nil fill-column 100))
  ((lisp-mode-hook scheme-mode-hook clojure-mode-hook)
   (setq-local tab-width 2))
  (emacs-lisp-mode-hook
   (setq-local tab-width 8))
  (before-save-hook
     (unless (eq major-mode 'markdown-mode)
       (delete-trailing-whitespace)))
  ((prog-mode-hook)
     (display-line-numbers-mode)
     (display-fill-column-indicator-mode))
  ((help-mode-hook sly-db-mode-hook) (visual-fill-column-mode))
  (css-mode-hook (electric-pair-local-mode)))

(global-visual-line-mode)
(global-auto-revert-mode t)

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
  (add-hook 'Info-mode-hook #'text-buf-wrap-setup)

  (leaf adaptive-wrap
    :config
    (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
    (add-hook 'org-mode-hook (lambda () (adaptive-wrap-prefix-mode -1))))

  (defun my/toggle-text-centering ()
    (interactive)
    (setq visual-fill-column-center-text (not visual-fill-column-center-text))))

(leaf ebnf-mode)

(leaf treemacs
  :config
  (defun turn-off-truncate-lines ()
    (toggle-truncate-lines 1))

  (add-hook 'treemacs-mode-hook 'turn-off-truncate-lines))
