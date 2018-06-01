
build:
	jbuilder build @install @demo #--dev

install uninstall doc:
	jbuilder $@

clean:
	jbuilder clean


.PHONY: build install uninstall doc clean
