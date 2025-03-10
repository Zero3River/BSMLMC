
.PHONY: all build run test clean

all: report.pdf build

report.pdf: *.tex lib/*.lhs test/*.lhs exec/*.lhs references.bib
	latexmk -pdf -synctex=1 -interaction=nonstopmode report

build:
	stack build

run:
	stack build && stack exec myprogram

test:
	stack test --coverage

clean:
	stack clean
	rm -f *.aux *.log *.out *.snm *.toc *.vrb *.nav *.synctex.gz *.blg *.bbl *.fdb_latexmk *.fls *.ind *.idx *.ilg *.bcf *.run.xml *.xdv
