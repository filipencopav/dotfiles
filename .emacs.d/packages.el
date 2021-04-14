(require 'cl-lib)
(require 'package)

;; Sets up melpa and elpa, decides about http or https
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

(defvar *my-packages* '() "List of packages which need to be installed.")
(setq *my-packages*
      '(slime
        company
        paredit
        avy
        yasnippet
        geiser
        highlight-parentheses
        dashboard
        meghanada
        gruvbox-theme))

(defun get-missing-packages (package-list)
  (cl-remove-if #'package-installed-p
                package-list))

(dolist (pkg (get-missing-packages *my-packages*))
  (package-install pkg))

(setq geiser-active-implementations '(guile))

;; Yasnippet
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; Dashboard
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-banner-logo-title "Welcome, traveler!")
(setq dashboard-startup-banner "/usr/share/icons/Faenza/apps/96/emacs.png")

;; Paren-face
(require 'paren-face)

(provide 'packages)
