TOP=../..
include ../Makefile.config
include ../Makefile.common

all: soli.byt

test:
	$(OCAMLRUN) soli.byt | cmp - soli.out
