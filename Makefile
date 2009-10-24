SCP = scp -C -p
TARBALL=/tmp/umons.tar.gz
WEB=w3.umh.ac.be:~/html/umons/template

OCAMLPACKS = netstring
GENERATED_PATT = index.%

index.cma: index.cmo
index.cmxa: index.cmx

index.ml index.mli: compile.exe
	./$< index.html

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