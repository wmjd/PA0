.PHONY: test

test: test.run
	./test.run

test.run: functions.ml test.ml
	ocamlfind ocamlc -o test.run -package oUnit -linkpkg -g functions.ml test.ml

.PHONY: clean

clean:
	-rm *.cmi
	-rm *.cmo
	-rm *.log
	-rm *.cache
	-rm test.run
