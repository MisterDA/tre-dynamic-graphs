pdf:
	latexmk -xelatex -shell-escape -interaction=batchmode

dyn: dyn.ml
	ocamlopt -g dyn.ml -o dyn

clean:
	latexmk -C
	rm -rf auto *.bbl *.run.xml
	rm -rf dyn *.cmi *.cmx *.o

.PHONY: pdf clean
