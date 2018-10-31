
build:
	dune build @install @demo

install uninstall:
	dune $@

doc:
	dune build @doc

clean:
	dune clean


.PHONY: build install uninstall doc clean
