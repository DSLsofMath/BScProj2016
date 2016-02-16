BIB=referenser.bib
CSL=../comp/ieee.csl

default: planeringsrapport.pdf

md: planeringsrapport.md

%.md:  %.tex
	pandoc -s --bibliography=$(BIB) --csl ieee.csl -t markdown_github --reference-links -f latex $< -o $@

%.pdf: %.tex
	latexmk -pdf $<

clean:
	latexmk -C
	rm planeringsrapport.md
