pandoc -s -S  --latex-engine=pdflatex --number-sections --table-of-contents --biblio biblio.bib --csl chicago-author-date.csl  analysis_notes.md -o analysis_notes.pdf
