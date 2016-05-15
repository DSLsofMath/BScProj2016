LATEXOPTS=-shell-escape
FILE=rapport
TEXHOME=$(HOME)/texmf/tex/latex

all: report

report:
	pdflatex $(LATEXOPTS) $(FILE).tex
	makeglossaries $(FILE).glo
	bibtex $(FILE)
	latexmk $(LATEXOPTS) -pdf $(FILE).tex

cleanup:
	rm *.{aux,bbl,bcf,blg,fdb_latexmk,fls,log,out}

clean: cleanup
	rm *.pdf

install: glossaries ieeetran

glossaries:
	mkdir -p "$(TEXHOME)/glossaries-swedish"
	cp include/packages/* $(TEXHOME)/glossaries-swedish/
	cd $(TEXHOME)/glossaries-swedish
	latex glossaries-swedish.ins > /dev/null

