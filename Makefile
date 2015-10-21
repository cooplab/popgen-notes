chapter_files := $(wildcard chapter*.tex)
html_files := $(patsubst %.tex,html/%.html,$(chapter_files))

all: popgen_notes.pdf

%.png : %.eps
	convert -density 300 $< -flatten $@

PHONY: clean all site

clean:
	rm -f popgen_notes.pdf
	rm -f html/*html

site: $(html_files)

popgen_notes.pdf: popgen_notes.tex
	latexmk $<

html/%.html: %.tex
	(cat template.tex; cat $<; echo "\\\\end{document}") | pandoc -s --mathjax --smart --to html5 --from latex > $@
