LATEXOPTS=-shell-escape
FILE=rapport

all: report

report:
	latex $(LATEXOPTS) $(FILE).tex
	makeglossaries $(FILE).glo
	bibtex $(FILE)
	latexmk $(LATEXOPTS) -pdf $(FILE).tex

cleanup:
	rm *.{aux,bbl,bcf,blg,fdb_latexmk,fls,log,out}

clean: cleanup
	rm *.pdf
