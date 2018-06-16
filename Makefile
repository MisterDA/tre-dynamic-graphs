pdf:
	latexmk -xelatex

dyn:
	ocamlopt -g dyn.ml -o dyn

clean:
	latexmk -C
	rm -rf auto *.bbl *.run.xml
	rm -rf dyn *.cmi *.cmx *.o

.PHONY: pdf clean
