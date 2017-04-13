.PHONY: all test install clean ios-install

OCAML_VERSION ?= 4.04.0
# only for ios

all: setup.data
	omake

setup.data:
	ocaml setup.ml -configure

test:
	./progs/unittests

install:
	omake reinstall

clean:
	[ ! -f .omakedb ] || omake clean
	find . -name "*.omc" | xargs rm
	rm -f setup.data
	rm -f .omakedb

ios-install: ios-install.x86 ios-install.amd64 ios-install.arm32 ios-install.arm64

ios-install.%:
	set -e; \
	eval `opam config env --switch=$(OCAML_VERSION)+ios+$*`; \
	{ [ ! -f .omakedb ] || omake clean; }; \
	find . -name "*.omc" | xargs rm; \
	rm -f setup.data; \
	rm -f .omakedb; \
	OCAMLFIND_TOOLCHAIN=ios ocaml setup.ml -configure; \
	OCAMLFIND_TOOLCHAIN=ios omake CMXS_ENABLED=false; \
	OCAMLFIND_TOOLCHAIN=ios omake CMXS_ENABLED=false reinstall
