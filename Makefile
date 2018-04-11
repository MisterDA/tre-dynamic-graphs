all:
	latexmk -xelatex -bibtex

clean:
	latexmk -C
	rm -rf auto *.bbl

.PHONY: all clean
