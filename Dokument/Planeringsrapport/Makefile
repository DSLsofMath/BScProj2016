default: planeringsrapport.pdf

%.pdf: %.tex
	latexmk -pdf $<

clean:
	latexmk -C
