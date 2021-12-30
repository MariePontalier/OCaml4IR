
build:
	@echo "\n==== COMPILING ====\n"
	ocamlbuild ftest.native
	ocamlbuild chocolatstest.native

symbols:
	@echo "\n==== SYMBOLS ====\n"
	cd src/ && ocamlc -c graph.mli graph.ml gfile.mli gfile.ml tools.mli tools.ml ford.mli ford.ml chocolats.mli chocolats.ml	

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n==== EXECUTING FTEST ====\n"
	./ftest.native graphs/graph1.txt 0 5 outfile
	dot -Tsvg outfile.dot > output.svg
	#@echo "\n==== RESULT ==== (content of outfile) \n"
	#@cat outfile

chocolats : build
	@echo "\n==== EXECUTING CHOCOLATSTEST ====\n"  	
	./chocolatstest.native chocolats/graph.txt 1 2 choco
	dot -Tsvg choco.dot > choco.svg



clean:
	-rm -rf _build/
	-rm ftest.native
