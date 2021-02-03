(require 'cl-lib)
(require 'package)

;; Sets up melpa and elpa, decides about http or https
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/"))))
    (package-initialize))

(defvar *my-packages*
      '(slime
        company
        paredit
        avy
        yasnippet
        geiser)
      "List of packages which need to be installed.")

(defun get-missing-packages (package-list)
  (cl-loop for pkg in package-list
           unless (package-installed-p pkg) collect pkg))

(dolist (pkg (get-missing-packages *my-packages*))
  (package-install pkg))

(require 'slime)

(yas-global-mode 1)


(provide 'packages)
