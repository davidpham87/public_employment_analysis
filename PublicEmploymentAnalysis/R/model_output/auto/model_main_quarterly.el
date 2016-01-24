(TeX-add-style-hook
 "model_main_quarterly"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "10pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=2cm")))
   (TeX-run-style-hooks
    "latex2e"
    "simple_statistic_quarterly"
    "article"
    "art10"
    "geometry"
    "tikz"
    "pdflscape")
   (TeX-add-symbols
    '("insertmodel" 2))))

