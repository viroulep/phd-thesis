PAPER=iwomp2016

all: pdf

pdf: bib $(PAPER).tex
	pdflatex $(PAPER).tex
	bibtex $(PAPER)
	pdflatex $(PAPER).tex
	pdflatex $(PAPER).tex

bib: Bib/paper.bib

clean:
	rm -rf *~ $(PAPER).pdf $(PAPER).cb $(PAPER).cb2 $(PAPER).aux $(PAPER).log $(PAPER).blg $(PAPER).bbl
