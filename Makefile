
.PHONY: build
build: setup.data
	ocaml setup.ml -build

setup.data: setup.ml
	ocaml setup.ml -configure --enable-tests

setup.ml: _oasis
	oasis setup

.PHONY: install
install: build
	ocaml setup.ml -install

.PHONY: test
test: build
	sudo ocaml setup.ml -test

.PHONY: uninstall
uninstall:
	rm -f ${BINDIR}/xe

.PHONY: clean
clean:
	ocaml setup.ml -clean

.PHONY: distclean
distclean: clean
	rm -f setup.log setup.data setup.ml _tags myocamlbuild.ml
