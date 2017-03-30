.PHONY: all install clean ios-install

OCAML_VERSION ?= 4.04.0
# only for ios

all: setup.data
	omake

setup.data:
	ocaml setup.ml -configure

install:
	omake reinstall

clean:
	[ ! -f .omakedb ] || omake clean
	find . -name "*.omc" | xargs rm
	rm -f setup.data
	rm -f .omakedb

ios-install: ios-install.i386 ios-install.amd64 ios-install.arm32 ios-install.arm64

ios-install.%:
	set -e; \
	{ [ ! -f .omakedb ] || omake clean; }; \
	find . -name "*.omc" | xargs rm; \
	rm -f setup.data; \
	rm -f .omakedb; \
	eval `opam config env --switch=$(OCAML_VERSION)+ios+$*`; \
	ocaml setup.ml -configure; \
	OCAMLFIND_TOOLCHAIN=ios omake CMXS_ENABLED=false; \
	OCAMLFIND_TOOLCHAIN=ios omake CMXS_ENABLED=false reinstall
