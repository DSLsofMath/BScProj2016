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

install: glossaries ieeetran minted

init:
	mkdir -p "$(TEXHOME)/"

glossaries: init
	echo "Installing glossaries..."
	cp -r include/packages/glossaries-swedish $(TEXHOME)/
	cd $(TEXHOME)/glossaries-swedish && \
	latex glossaries-swedish.ins > /dev/null

ieeetran: init
	echo "Copying ieeetran.bst into texmf/..."
	mkdir -p "$(HOME)/texmf/bibtex/bst/ieeetran/"
	cp include/ieeetran.bst "$(HOME)/texmf/bibtex/bst/ieeetran/"

minted: init
	echo "Installing minted..."
	cp -r include/packages/minted $(TEXHOME)/
	cd $(TEXHOME)/minted && make > /dev/null
