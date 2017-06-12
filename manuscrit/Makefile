all: FORCE

FORCE:
	pdflatex Master.tex
	bibtex Master
	pdflatex Master.tex
	pdflatex Master.tex

clean:
	rm -f *.aux *.brf *.log *.bbl *.blg *.dvi *.ps *.pdf *.*toc *.kilepr *.backup *.maf *.mtc* *.out
