all:
	latexmk -xelatex

clean:
	latexmk -C
	rm -rf auto *.bbl *.run.xml

.PHONY: all clean
