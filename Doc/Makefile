SOURCES = \
		Makefile \
		blankpage.tex \
		forvis_reading_guide.tex \
		forvis_reading_guide.bib \
		Extracted

.PHONY: default
default: $(SOURCES)  extract
	pdflatex  forvis_reading_guide

.PHONY: full
full: $(SOURCES)  extract
	pdflatex  forvis_reading_guide
	bibtex    forvis_reading_guide
	pdflatex  forvis_reading_guide
	pdflatex  forvis_reading_guide

.PHONY: bib
bib:
	bibtex  forvis_reading_guide

.PHONY: extract
extract: Extracted
	./Extract_latex_from_hs.py  ../src  Extracted

Extracted:
	mkdir -p  Extracted

.PHONY: clean
clean:
	rm -f  *~  *.toc  *.aux  *.blg  *.bbl  *.log  *.out  *.dvi

.PHONY: fullclean
full_clean:
	make clean
	rm  -r -f  forvis_reading_guide.pdf  Extracted
