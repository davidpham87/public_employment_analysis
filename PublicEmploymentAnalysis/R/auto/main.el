(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "10pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=2.5cm")))
   (TeX-run-style-hooks
    "latex2e"
    "test"
    "article"
    "art10"
    "geometry"
    "tikz"
    "pdflscape")))

