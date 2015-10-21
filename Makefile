all: popgen_notes.pdf

%.png : %.eps
	convert -density 300 $< -flatten $@

PHONY: clean all

clean:
	rm -f popgen_notes.pdf

popgen_notes.pdf: popgen_notes.tex
	latexmk $<

%.tex: html/%.html
	pandoc -s --mathjax --smart --to html5 $< > $@
