(TeX-add-style-hook
 "main_simple_model_quarterly"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "10pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=2.5cm")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "geometry"
    "tikz"
    "pdflscape"
    "pdfpages"
    "graphicx")
   (TeX-add-symbols
    '("insertplot" 2)))
 :latex)

