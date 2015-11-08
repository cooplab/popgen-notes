chapter_files := $(wildcard chapter*.tex)
html_files := $(patsubst %.tex,html/%.html,$(chapter_files))

all: popgen_notes.pdf

%.png : %.eps
	convert -density 300 $< -flatten $@

PHONY: clean all site

clean:
	rm -f popgen_notes.pdf
	rm -f html/chapter-*html
	latexmk -c

site: html/index.html figures
	cp -r figures/*png html/figures
	pandoc -s --mathjax --toc --css css/style.css --smart --to html5 --from latex popgen_notes.tex > html/index.html

browse: site
	open html/index.html
 
popgen_notes.pdf: popgen_notes.tex
	latexmk $<


deploy: html/
	# using https://github.com/X1011/git-directory-deploy
	./deploy.sh
