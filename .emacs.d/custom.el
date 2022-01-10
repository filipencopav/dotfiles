
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beacon-blink-delay 0.3)
 '(beacon-blink-duration 0.4)
 '(beacon-blink-when-focused t)
 '(beacon-blink-when-point-moves-vertically 10)
 '(beacon-size 20)
 '(company-idle-delay 0.4)
 '(dashboard-banner-logo-title "O U R macs")
 '(dashboard-center-content t)
 '(dashboard-items '((recents . 4) (bookmarks . 4)))
 '(dashboard-page-separator "
")
 '(highlight-parentheses-delay 0)
 '(inferior-lisp-program "sbcl")
 '(initial-buffer-choice 'my/choose-initial-buffer)
 '(org-bullets-bullet-list '("●" "*" "•" "·"))
 '(org-file-apps
   '(("\\.pdf\\'" . "xdg-open %s")
     (auto-mode . emacs)
     (directory . default)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)))
 '(org-roam-complete-everywhere t t)
 '(package-selected-packages
   '(magit ido-vertical-mode smooth-scrolling csharp-mode org-contrib paredit rustic highlight-parentheses company yasnippet dashboard avy sly editorconfig scss-mode org-bullets org-roam selectrum marginalia beacon gruvbox-theme eglot el-get leaf-keywords feather))
 '(scss-compile-at-save t)
 '(sly-lisp-implementations
   '((sbcl
      ("sbcl" "--core" "/home/pavel/.emacs.d/slynk/sbcl.core-for-sly"))) t)
 '(smooth-scroll-margin 5)
 '(yas-snippet-dirs '("~/.emacs.d/snippets")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
