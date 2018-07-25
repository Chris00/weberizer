
build:
	dune build @install @demo

install uninstall doc:
	dune $@

clean:
	dune clean


.PHONY: build install uninstall doc clean
