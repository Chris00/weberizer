SCP = scp -C -p
TARBALL=/tmp/umons.tar.gz
WEB=w3.umh.ac.be:~/html/umons/template

SOURCES = UMONS.ml template.ml translate.ml

OCAMLC_FLAGS = -g -dtypes
OCAMLPACKS = netstring,unix,str
GENERATED_PATT = UMONS.%

.PHONY: all byte native
all: byte native
byte: $(SOURCES:.ml=.cma)
native: $(SOURCES:.ml=.cmxa)

UMONS.cma: UMONS.cmo
UMONS.cmxa: UMONS.cmx

UMONS.ml UMONS.mli: compile.exe UMONS.html UMONS.html.ml UMONS.html.mli
	./$< UMONS.html

compile.exe: template.cma

template.cma: template.cmo
template.cmxa: template.cmx
translate.cma translate.cmxa: OCAMLPACKS:=$(OCAMLPACKS),netclient
translate.cma: translate.cmo
translate.cmxa: translate.cmx

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