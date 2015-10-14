
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

TEST=ocaml setup.ml -test -runner sequential
THREAD_TEST=$(shell ${TEST} -list-test | grep thread_safe)
.PHONY: test
test: build
	${TEST}
	./parallel_run.native -- ${TEST} -only-test ${THREAD_TEST}

.PHONY: uninstall
uninstall:
	rm -f ${BINDIR}/xe

.PHONY: clean
clean:
	ocaml setup.ml -clean

.PHONY: distclean
distclean: clean
	rm -f setup.log setup.data setup.ml _tags myocamlbuild.ml
