(TeX-add-style-hook
 "model_main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "10pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=2.5cm")))
   (TeX-run-style-hooks
    "latex2e"
    "simple_statistic"
    "article"
    "art10"
    "geometry"
    "tikz"
    "pdflscape")
   (TeX-add-symbols
    '("insertmodel" 2))
   (LaTeX-add-environments
    '("innerlist" LaTeX-env-args ["argument"] 0))))

