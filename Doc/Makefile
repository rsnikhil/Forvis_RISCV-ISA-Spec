SOURCES = \
		Makefile \
		blankpage.tex \
		forvis_reading_guide.tex \
		forvis_reading_guide.bib

.PHONY: default
default: $(SOURCES)
	pdflatex  forvis_reading_guide

.PHONY: full
full: $(SOURCES)
	pdflatex  forvis_reading_guide
	bibtex    forvis_reading_guide
	pdflatex  forvis_reading_guide
	pdflatex  forvis_reading_guide

.PHONY: bib
bib:
	bibtex  forvis_reading_guide

.PHONY: extract
extract:
	mkdir -p  Extracted
	./Extract_latex_from_hs.py  ../src  Extracted

.PHONY: clean
clean:
	rm -f  *~  *.toc  *.aux  *.blg  *.bbl  *.log  *.out  *.dvi

.PHONY: fullclean
full_clean:
	make clean
	rm  forvis_reading_guide.pdf
