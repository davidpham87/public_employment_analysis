pandoc -s -S  --latex-engine=pdflatex --number-sections --table-of-contents --biblio biblio.bib --csl chicago-author-date.csl  public_employment_notes.md -o public_employment_notes.pdf
