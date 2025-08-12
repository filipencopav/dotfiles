

(require 'pixel-scroll)

(defun p/20%-scroll-up ()
  (interactive)
  (let ((h (window-pixel-height)))
    (pixel-scroll-precision-interpolate (/ h 5))))

(defun p/20%-scroll-down ()
  (interactive)
  (let ((h (window-pixel-height)))
    (pixel-scroll-precision-interpolate (- (/ h 5)))))

(defun p/switch-to-normal ()
  (interactive)
  (ryo-modal-mode 1))

(defun p/switch-to-insert ()
  (interactive)
  (ryo-modal-mode -1))

(defun p/insert-space ()
  (interactive)
  (insert-char ?\s))

(defun p/avy-goto-after-char (char &optional arg)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-goto-char char arg)
  (forward-char))

(defun p/kill-region-or-line (&optional beg end)
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if beg
      (kill-region beg end)
    (kill-whole-line)))

(defun p/copy-region-or-line (&optional beg end)
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (save-excursion
    (let ((beg (if beg
                   beg
                 (progn (beginning-of-line)
                        (point))))
          (end (if beg
                   end
                 (progn (end-of-line)
                        (point)))))
      (kill-ring-save beg end))))

(defun p/config-edit ()
  "Find config.org"
  (interactive)
  (find-file *config.org-location*))

(defun p/config-reload ()
  "Reload the configuration file"
  (interactive)
  (org-babel-load-file *config.org-location*))

(leaf avy)

(define-key ctl-x-map (kbd "k") 'kill-current-buffer)
(define-key ctl-x-map (kbd "C-b") nil)
(define-key global-map (kbd "C-t") 'set-mark-command)
(define-key esc-map (kbd "j") 'avy-goto-char)
(define-key esc-map (kbd "c") 'p/copy-region-or-line)

;; remap C-x to something else, map C-k to `ctl-x-map'
(define-key global-map (kbd "C-x") 'p/kill-region-or-line)
(define-key global-map (kbd "C-k") ctl-x-map)
(customize-set-variable
 'gud-key-prefix
 (thread-first
   (where-is-internal ctl-x-map)
   (car)
   (concat (kbd "C-a"))))


(define-key org-mode-map (kbd "C-x") nil)
(define-key org-src-mode-map (kbd "C-x") nil)

(defvar p/ctl-z-map (make-keymap))
(defvar p/ctl-z-c-map (make-keymap))

(define-key global-map (kbd "C-z") p/ctl-z-map)
(define-key p/ctl-z-map (kbd "c") p/ctl-z-c-map)
(define-key p/ctl-z-c-map (kbd "e") 'p/config-edit)
(define-key p/ctl-z-c-map (kbd "r") 'p/config-reload)

(define-key global-map (kbd "C-;") 'save-buffer)

(add-hook 'after-init-hook (lambda () (load-theme 'modus-vivendi t)))

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default left-fringe-width 0)
(setq-default right-fringe-width 0)

(leaf vertico
  :require (t vertico-directory)
  :bind
  (:vertico-map
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char))
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :config
  (vertico-mode 1))

(leaf marginalia
  :config
  (marginalia-mode 1))

(customize-set-variable 'pixel-scroll-precision-interpolate-mice t)
(customize-set-variable 'pixel-scroll-precision-interpolate-page t)
(pixel-scroll-precision-mode)

(defun p/length-mode-line (mode-line)
  (length (format-mode-line mode-line)))

(defun p/simple-mode-line-render (left right)
  "Return a string of `window-width' length. Containing LEFT, and RIGHT aligned respectively."
  (let* ((window-width (window-total-width))
         (mode-line-lengths (apply #'+ (mapcar #'p/length-mode-line (list left right))))
         (available-width (- window-width mode-line-lengths))
         (format-string (format "%%%ds" available-width)))
    (append left (list (format format-string "")) right)))

(setq-default
 mode-line-format
 '((:eval
    (p/simple-mode-line-render
     ;; Left
     '("»"
       " %b "
       (:eval (cond (buffer-read-only "[RO]") ((buffer-modified-p) "[+]"))))
     ;; Right
     '("line %3l, col %3c"
       " «")))))

(defun p/apply-emoji-font ()
  (set-fontset-font t 'emoji (font-spec :family "Twitter Color Emoji" :size 13) nil 'prepend))
(add-hook 'server-after-make-frame-hook #'p/apply-emoji-font)

(defun p/set-modal-cursor ()
  (setq-local cursor-type
              (if ryo-modal-mode
                  'box
                'bar)))

(add-hook 'ryo-modal-mode-hook 'p/set-modal-cursor)

(setq-default cursor-type 'bar)

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
  )

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)

;; do this especially on Windows, else python output problem
(set-terminal-coding-system 'utf-8-unix)

(setq-default default-input-method "russian-computer")

(require 'ibuffer)
(define-key ibuffer-mode-map (kbd "k") 'next-line)
(define-key ibuffer-mode-map (kbd "i") 'previous-line)

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
(add-hook 'ibuffer-mode-hook 'p/ibuffer-setup)
(defun p/ibuffer-setup ()
  (ibuffer-switch-to-saved-filter-groups "default")
  (setq ibuffer-hidden-filter-groups (list "org agenda" "emacs"))
  (ibuffer-update nil t)
  (setq-local truncate-partial-width-windows nil)
  (visual-fill-column-mode -1)
  (visual-line-mode -1)
  (toggle-truncate-lines 1))

(leaf combobulate
  :elpaca (combobulate :url "https://github.com/mickeynp/combobulate.git")
  :after (go-ts-mode)
  :hook ((go-ts-mode-hook) . combobulate-mode)
  )

(leaf orderless
  :setq
  (completion-styles . '(basic orderless))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles partial-completion)))))

(leaf corfu
  :custom
  (corfu-auto . t)
  (corfu-quit-no-match . t)
  (corfu-auto-delay . 0.5)
  :config
  (global-corfu-mode))

(leaf org
  :custom
  (org-babel-load-languages . '((emacs-lisp . t) (lisp . t)))
  (org-todo-keywords . '((sequence "TODO" "|" "DONE" "CANCELLED" "SUSPENDED")))
  (org-file-apps
   '(("\\.pdf\\'" . "xdg-open %s")
     (auto-mode . emacs)
     (directory . default)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)))
  ((org-confirm-babel-evaluate
    org-capture-bookmark
    org-link-descriptive
    org-html-head-include-default-style
    org-adapt-indentation
    org-startup-truncated)
   . nil)
  (org-src-window-setup . 'current-window)
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
  :bind
  :config
  (define-key p/ctl-z-c-map (kbd "c") 'org-capture)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

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
  (setq org-roam-directory (thread-first
                             "org/roam/"
                             (expand-file-name *emacs-config-location*)
                             (file-truename)))
  (define-prefix-command 'p/org-roam-commands)
  (define-key p/org-roam-commands (kbd "f") 'org-roam-node-find)
  (define-key p/org-roam-commands (kbd "i") 'org-roam-node-insert)
  (define-key p/ctl-z-map (kbd "n") p/org-roam-commands)
  (org-roam-setup))

(leaf org-roam-ui
  :require (org-roam t)
  :after org-roam)

(setq column-number-mode t
      split-width-threshold 120
      confirm-kill-process nil)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 100
              c-default-style '((c-mode . "bsd"))
              c-basic-offset tab-width
              cperl-indent-level tab-width
              display-line-numbers-width 3)

(defmacro p/add-hooks (&rest pairs)
  "Adds lambdas to the hooks of the specified modes. Takes a list of lists, the first element may be either a symbol (hook variable) or a list of symbols. The rest of the elements are "
  `(progn
     ,@(mapcan
        (lambda (pair)
          (let* ((mode-or-modes (car pair))
                 (body (cdr pair))
                 (name (thread-last
                         mode-or-modes
                         (sxhash)
                         (number-to-string)
                         (concat "p/add-hooks.")
                         (read-from-string)
                         (car))))
            `((defun ,name () ,@ body)
              ,@(mapcar (lambda (x) `(add-hook ',x ',name))
                        (if (listp mode-or-modes)
                            mode-or-modes
                          (list mode-or-modes))))))
        pairs)))

(p/add-hooks
 ((lisp-mode-hook scheme-mode-hook emacs-lisp-mode-hook clojure-mode-hook)
  (setq indent-tabs-mode nil fill-column 100))
 ((lisp-mode-hook scheme-mode-hook clojure-mode-hook)
  (setq-local tab-width 2))
 (emacs-lisp-mode-hook
  (setq-local tab-width 8))
 (before-save-hook
  (unless (eq major-mode 'markdown-mode)
    (delete-trailing-whitespace)))
 (prog-mode-hook
  (display-line-numbers-mode)
  (display-fill-column-indicator-mode))
 ((help-mode-hook sly-db-mode-hook)
  (visual-fill-column-mode))
 (css-mode-hook
  (electric-pair-local-mode))
 (compilation-finish-functions
  (when (null (string-match ".*exited abnormally.*" str))
    (let ((win (get-buffer-window buf 'visible)))
      (when win (delete-window win))))))

(leaf tree-sitter
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (global-tree-sitter-mode))
(leaf tree-sitter-langs)
(leaf treesit-auto
  :after (tree-sitter tree-sitter-langs)
  :custom
  '(treesit-auto-install . t)
  (treesit-auto-langs . '(awk bash bibtex c c-sharp clojure cmake commonlisp
                              cpp css dart dockerfile elixir glsl go gomod
                              heex html java javascript
                              json julia kotlin lua make markdown nix org perl
                              proto python r ruby rust scala sql surface
                              toml tsx typescript typst
                              vhdl vue wast wat wgsl yaml))

  :config
  (global-treesit-auto-mode)
  (add-to-list
   'treesit-language-source-alist
   '(markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                 "update" "tree-sitter-markdown/src")))
  (add-to-list 'treesit-language-source-alist
               '(astro "https://github.com/virchau13/tree-sitter-astro")))

(leaf eglot
  :elpaca nil
  :config
  (defun p/eglot-hooks ()
    (when (eglot-managed-p)
      (add-hook 'before-save-hook 'eglot-format nil t)))
  (add-hook 'eglot-managed-mode-hook 'p/eglot-hooks))

(leaf reformatter)

(leaf editorconfig
  :config
  (editorconfig-mode 1))

(leaf yasnippet
  :hook (prog-mode-hook . yas-minor-mode)
  :config
  (setq yas-snippet-dirs (thread-last
                           *emacs-config-location*
                           (expand-file-name "snippets")
                           (list)))
  (yas-reload-all))

(leaf eldoc-box
  :init
  (defun p/eldoc-box-scroll-up ()
    "Scroll up in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-down 3))))
  (defun p/eldoc-box-scroll-down ()
    "Scroll down in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-up 3))))
  :after eglot
  :custom
  (eldoc-box-max-pixel-height . 200)
  :bind ((:eglot-mode-map
          ("M-p" . p/eldoc-box-scroll-up)
          ("M-n" . p/eldoc-box-scroll-down)))

  :config
  (defun p/eldoc-hooks ()
    (interactive)
    (eldoc-box-hover-mode))
  (add-hook 'eldoc-mode-hook 'p/eldoc-hooks))

(leaf go-ts-mode
  :after (eglot reformatter tree-sitter tree-sitter-langs treesit-auto)
  :elpaca nil t
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
    (add-to-list
     'eglot-server-programs
     `((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode)
       "gopls" "-logfile"
       ,(expand-file-name "gopls/emacs.log" cache-dir))))

  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))
  (add-hook 'project-find-functions #'project-find-go-module))

(leaf nix-ts-mode
  :after (eglot)
  :mode "\\.nix\\'"
  :config
  (add-to-list 'eglot-server-programs '(nix-ts-mode "nixd"))
  (add-hook 'nix-ts-mode-hook 'eglot-ensure))

(leaf typescript-ts-mode
  :after (eglot)
  :elpaca nil
  :config
  (setq js-indent-level 2)
  (setq typescript-indent-level js-indent-level)
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode typescript-mode) "deno" "lsp"))
  (add-to-list 'typescript-ts-mode-hook #'eglot-ensure))

(leaf astro-ts-mode
  :after (eglot treesit-auto typescript-ts-mode)
  :config
  (let ((astro-recipe (make-treesit-auto-recipe
                       :lang 'astro
                       :ts-mode 'astro-ts-mode
                       :url "https://github.com/virchau13/tree-sitter-astro"
                       :revision nil
                       :source-dir nil
                       :ext "\\.astro\\'")))
    (add-to-list 'treesit-auto-recipe-list astro-recipe)
    (add-to-list 'treesit-auto-langs 'astro))
  (add-to-list
   'eglot-server-programs
   '(astro-ts-mode
     "astro-ls" "--stdio"
     :initializationOptions
     (:typescript (:tsdk "./node_modules/typescript/lib"))))
  (add-hook 'astro-ts-mode-hook 'eglot-ensure))

(leaf vue-ts-mode
  :after (eglot treesit-auto typescript-ts-mode)
  :elpaca (vue-ts-mode :url "https://github.com/8uff3r/vue-ts-mode.git")
  :config
  (let ((vue-recipe (make-treesit-auto-recipe
                     :lang 'vue
                     :ts-mode 'vue-ts-mode
                     :url "https://github.com/ikatyang/tree-sitter-vue"
                     :revision nil
                     :source-dir nil
                     :ext "\\.vue\\'")))
    (add-to-list 'treesit-auto-recipe-list vue-recipe)
    (add-to-list 'treesit-auto-langs 'vue))
  (add-to-list
   'eglot-server-programs
   '(vue-ts-mode
     "vue-language-server" "--stdio"))
  (add-hook 'vue-ts-mode-hook 'eglot-ensure))

(leaf erlang
  :elpaca (erlang :version (lambda (_) "28.0")))

(leaf erlang-ts
  :require '(t erlang-start)
  :after (eglot erlang treesit-auto)
  :hook
  (erlang-mode-hook . eglot-ensure)
  :mode
  ("/?rebar\\.config\\'" . erlang-ts-mode)
  ("\\.erl\\'" . erlang-ts-mode)
  :setq (erlang-electric-commands . '(erlang-electric-gt))
  :config
  (add-to-list 'major-mode-remap-alist '(erlang-mode . erlang-ts-mode))
  (let ((erlang-recipe (make-treesit-auto-recipe
                         :lang 'erlang
                         :ts-mode 'erlang-ts-mode
                         :url "https://github.com/WhatsApp/tree-sitter-erlang"
                         :revision nil
                         :source-dir nil
                         :ext "\\.erl\\'")))
    (add-to-list 'treesit-auto-recipe-list erlang-recipe)
    (add-to-list 'treesit-auto-langs 'erlang))
  (add-to-list 'eglot-server-programs
               '(erlang-mode "elp" "server"))
  (add-hook 'erlang-ts-mode-hook 'eglot-ensure))

(leaf clojure-ts-mode
  :after (eldoc-box)

  :setq
  (clojure-ts-indent-style . 'fixed)

  :bind (:clojure-ts-mode-map
         ("M-p" . p/eldoc-box-scroll-up)
         ("M-n" . p/eldoc-box-scroll-down))

  :config
  (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode)))

(leaf cider
  :require (t cider-eval)
  :after (clojure-ts-mode)
  :config
  (define-key cider-mode-map [remap eval-last-sexp] 'cider-eval-last-sexp)
  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)
  (setq cider-repl-display-output-before-window-boundaries t)
  (setq cider-show-error-buffer t)
  (setq cider-redirect-server-output-to-repl t)
  ;; do not indent single ; character
  (add-hook 'clojure-mode-hook (lambda () (setq-local comment-column 0)))

  (setq cider-clojure-cli-aliases ":dev"))

(leaf web-mode
  :config
  (defun p/replace-mhtml (cons)
    (if (member (cdr cons) '(mhtml-mode))
        (cons (car cons) 'web-mode)
      cons))
  (setq auto-mode-alist (mapcar #'p/replace-mhtml auto-mode-alist))
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

  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

  (defun p/web-mode-hooks ()
    "Hooks for Web mode."
    )

  (add-hook 'web-mode-hook 'p/web-mode-hooks))

(defvar *sly-image-location*
  (expand-file-name "lisp/sbcl.core-for-sly" *emacs-config-location*))

(leaf sly
  :after (org)
  :elpaca (sly :repo "filipencopav/sly"
               :branch "fix-ctl-x-keymap")
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
  (defun p/generate-sly-image ()
    (interactive)
    (compile
     (format
      "sbcl \\
        --eval \"(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect
                                  sb-cltl2 asdf))\" \\
        --eval '(save-lisp-and-die \"sbcl.core-for-sly\")'"
      (expand-file-name "sly/slynk/slynk-loader.lisp" *emacs-config-location*)
      *sly-image-location*)))

  (with-eval-after-load 'sly-mrepl
    (define-key sly-mrepl-mode-map [remap eval-last-sexp] 'sly-eval-last-expression))
  (define-key sly-mode-map [remap eval-last-sexp] 'sly-eval-last-expression))

(leaf dockerfile-ts-mode
  :elpaca nil
  :mode ("\\[dD]ockerfile\\'" . dockerfile-ts-mode))

(leaf yaml-ts-mode
  :elpaca nil
  :mode ("\\.ya?ml\\'" . yaml-ts-mode))

(leaf protobuf-ts-mode
  :mode ("\\.proto\\'" . protobuf-ts-mode))

(leaf typst-ts-mode
  :elpaca (typst-ts-mode
           :url "https://codeberg.org/meow_king/typst-ts-mode.git")
  :after tree-sitter)

(leaf treemacs
  :config
  (defun p/turn-on-truncate-lines ()
    (toggle-truncate-lines 1))

  (add-hook 'treemacs-mode-hook 'p/turn-on-truncate-lines))
