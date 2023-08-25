;;; init.el --- -*- lexical-binding: t; -*-

(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

;;; leaf-install-code
(require 'package)
(eval-and-compile
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archive-priorities '("melpa-stable" . 1))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (unless package-archive-contents
    (package-refresh-contents))

  (setq leaf-defaults '(:require t :ensure t))

  (leaf leaf-keywords
    :init
    (leaf el-get :ensure t)
    :config
    (leaf-keywords-init)))
;;; leaf-install-code

(load-file "~/.emacs.d/config.el")
(xah-fly-keys)
