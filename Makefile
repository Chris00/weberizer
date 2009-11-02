SCP = scp -C -p
TARBALL=/tmp/umons.tar.gz
WEB=w3.umh.ac.be:~/html/umons/template

OCAMLC_FLAGS = -g -dtypes
OCAMLPACKS = netstring,unix
GENERATED_PATT = UMONS.%

.PHONY: all byte native
all: byte native
byte: UMONS.cma template.cma
native: UMONS.cmxa template.cmxa

UMONS.cma: UMONS.cmo
UMONS.cmxa: UMONS.cmx

UMONS.ml UMONS.mli: compile.exe UMONS.html
	./$< UMONS.html

compile.exe: template.cma

template.cma: template.cmo
template.cmxa: template.cmx


FILES=$(wildcard *.html *.css *.jpg *.png *.gif)
.PHONY: tar
tar:
	tar -zcf $(TARBALL) $(FILES)

.PHONY: upload upload-html
upload:
	$(SCP) $(FILES) $(WEB)

upload-html:
	$(SCP) $(filter %.html %.css, $(FILES)) $(WEB)

include Makefile.ocaml

dist-clean::
	$(RM) UMONS.ml UMONS.mli

# Dependencies for the generated files
UMONS.cmo UMONS.cmx: UMONS.cmi