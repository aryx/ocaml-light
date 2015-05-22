TOP=../..

include $(TOP)/config/Makefile

CAMLRUN=$(TOP)/boot/ocamlrun
CAMLC=$(CAMLRUN) $(TOP)/boot/ocamlc -I $(TOP)/boot
CAMLLEX=$(CAMLRUN) $(TOP)/boot/ocamllex
INCLUDES=-I $(TOP)/utils -I $(TOP)/parsing -I $(TOP)/typing -I $(TOP)/bytecomp -I $(TOP)/asmcomp
COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES)

all: ocamldep

# The dependency generator

CAMLDEP=ocamldep.cmo

ocamldep: $(CAMLDEP)
	$(CAMLC) $(LINKFLAGS) -o ocamldep misc.cmo $(CAMLDEP)

#pad: better not deleting it
#clean::
#	rm -f ocamldep

ocamldep.ml: ocamldep.mll
	$(CAMLLEX) ocamldep.mll

clean::
	rm -f ocamldep.ml

install::
	cp ocamldep $(BINDIR)/ocamldep

beforedepend:: ocamldep.ml

# Common stuff

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

clean::
	rm -f *.cmo *.cmi

depend: beforedepend
	$(CAMLRUN) ./ocamldep $(INCLUDES) *.mli *.ml > .depend

include .depend
