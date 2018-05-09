PAPER=phd_defense

all: pdf

pdf: $(PAPER).tex
	pdflatex $(PAPER).tex
	pdflatex $(PAPER).tex

clean:
	rm -rf *~ $(PAPER).pdf $(PAPER).cb $(PAPER).cb2 $(PAPER).aux $(PAPER).log $(PAPER).blg $(PAPER).bbl $(PAPER).vrb $(PAPER).snm $(PAPER).toc $(PAPER).nav $(PAPER).out
