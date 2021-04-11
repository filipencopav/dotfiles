(require 'cl-lib)
(require 'package)

;; Sets up melpa and elpa, decides about http or https
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (package-initialize))
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
        dashboard))

(defun get-missing-packages (package-list)
  (cl-loop for pkg in package-list
              unless (package-installed-p pkg) collect pkg))

(dolist (pkg (get-missing-packages *my-packages*))
  (package-install pkg))

(setq geiser-active-implementations '(guile))
(yas-global-mode 1)

;; Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-banner-logo-title "Welcome, traveler!")
(setq dashboard-startup-banner "/usr/share/icons/Faenza/apps/96/emacs.png")

(provide 'packages)
