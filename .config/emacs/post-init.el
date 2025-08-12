;;; post-init.el --- -*- lexical-binding: t; -*-

(defvar *emacs-config-location*
  (expand-file-name "emacs" (getenv "XDG_CONFIG_HOME")))
(defvar *config.org-location*
  (expand-file-name "config.org" *emacs-config-location*))

(eval-when-compile
    (require 'org-element)
    (require 'org-tempo)
    (require 'org))

(org-babel-load-file *config.org-location*)
