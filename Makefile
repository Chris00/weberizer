SCP = scp -C -p
TARBALL=/tmp/umons.tar.gz
WEB=w3.umh.ac.be:~/html/umons/template

FILES=$(wildcard *.html *.css *.jpg *.png *.gif)
.PHONY: tar
tar:
	tar -zcf $(TARBALL) $(FILES)

.PHONY: upload upload-html
upload:
	$(SCP) $(FILES) $(WEB)

upload-html:
	$(SCP) $(filter %.html %.css, $(FILES)) $(WEB)