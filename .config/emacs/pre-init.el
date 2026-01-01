;;; ...  -*- lexical-binding: t -*-

;;; Reducing clutter in ~/.emacs.d by redirecting files to ~/.emacs.d/var/
(setq user-emacs-directory
      (expand-file-name "var/" minimal-emacs-user-directory))


;; Set emacs build date if on nixos for elpaca
(when (and (file-exists-p "/etc/os-release")
           (with-temp-buffer
             (insert-file-contents "/etc/os-release")
             (re-search-forward "^ID=nixos$" nil t)))
  (setq elpaca-core-date "2025-08-06"))

;;; START elpaca install code
(setq package-enable-at-startup nil)
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))


(add-hook 'after-init-hook #'elpaca-process-queues)

(defun my-fix (orig-fn &rest args)
  "Return an alist of MELPA package metadata."
  (require 'json)
  (let* ((json-object-type 'alist)
         (metadata (json-read-file
                    (expand-file-name "../melpa-archive.json"
                                      user-emacs-directory))))
    metadata))

(advice-add 'elpaca-menu-melpa--metadata :around #'my-fix)

(elpaca `(,@elpaca-order))

(elpaca-wait)
;;; END elpaca install code

;;; START leaf install code
(unless (and (elpaca-installed-p 'leaf)
             (elpaca-installed-p 'leaf-keywords))
  (elpaca leaf (require 'leaf))
  (elpaca leaf-keywords (require 'leaf-keywords)))

(elpaca-wait)

(leaf-keywords-init)

(setq leaf-defaults '(:require t :elpaca t))
;;; END leaf install code
