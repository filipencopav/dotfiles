(require 'org)

(add-to-list 'org-modules 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-latex-packages-alist '("" "babel" t ("pdflatex")))
(add-to-list 'org-latex-packages-alist '("" "minted" t ("pdflatex")))

(setq org-latex-minted-options '(("breaklines" "true")
                                 ("breakanywhere" "true")))

(setq
 org-adapt-indentation nil
 org-latex-title-command nil
 org-latex-listings 'minted
 org-latex-pdf-process
 '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
 org-latex-minted-options '(("breaklines" "true") ("breakanywhere" "true"))
 )

(provide 'org-config.el)
