PKGNAME	    = $(shell oasis query name)
PKGVERSION  = $(shell oasis query version)
PKG_TARBALL = $(PKGNAME)-$(PKGVERSION).tar.gz

WEB = forge.ocamlcore.org:/home/groups/ocamlweb/htdocs/weberizer/

DISTFILES   = README.md _oasis \
  Makefile setup.ml _tags src/ demo/

.PHONY: all byte native configure doc install uninstall reinstall upload-doc

all byte native setup.log: configure
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml
	ocaml $< -configure

setup.ml: _oasis
	oasis setup -setup-update dynamic

doc install uninstall: setup.log
	ocaml setup.ml -$@

reinstall: setup.log
	ocamlfind remove $(PKGNAME)
	ocaml setup.ml -$@

upload-doc: doc
	scp -C -p -r _build/API.docdir/ $(WEB)

# Make a tarball
.PHONY: dist tar
dist tar: $(DISTFILES)
	mkdir $(PKGNAME)-$(PKGVERSION)
	cp --parents -r $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/
	tar -zcvf $(PKG_TARBALL) $(PKGNAME)-$(PKGVERSION)
	rm -rf $(PKGNAME)-$(PKGVERSION)

.PHONY: clean distclean
clean::
	ocaml setup.ml -clean
	$(RM) $(PKG_TARBALL)

distclean:
	ocaml setup.ml -distclean
	$(RM) $(wildcard *.ba[0-9] *.bak *~ *.odocl)
	$(RM) setup.ml myocamlbuild.ml AUTHORS.txt
