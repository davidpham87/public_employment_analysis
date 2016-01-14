pandoc -s -S  --latex-engine=pdflatex --number-sections --table-of-contents --biblio biblio.bib --csl chicago-author-date.csl -V geometry:margin=3cm analysis_notes.org -o analysis_notes.pdf
