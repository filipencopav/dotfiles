;;; init.el --- -*- lexical-binding: t; -*-

(setq startup/gc-cons-threshold (expt 2 23))
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

;;; leaf-install-code
(require 'package)
(eval-and-compile
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (setq leaf-defaults '(:require t :ensure t))

  (leaf leaf-keywords))

;;; leaf-install-code

(defvar *emacs-config-location*
  (expand-file-name "emacs" (getenv "XDG_CONFIG_HOME")))
(defvar *config.el-location*
  (expand-file-name "config.el" *emacs-config-location*))

(load-file *config.el-location*)
(xah-fly-keys)
