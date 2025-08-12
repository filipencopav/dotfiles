;; Need this so packages are fetched from the more fresh melpa
(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 70)
                          ("nongnu" . 60)
                          ("melpa"  . 99)))
