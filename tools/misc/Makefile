TOP=../..

include $(TOP)/config/Makefile

CAMLRUN=$(TOP)/boot/ocamlrun
CAMLC=$(CAMLRUN) $(TOP)/boot/ocamlc -I $(TOP)/boot
CAMLLEX=$(CAMLRUN) $(TOP)/boot/ocamllex
INCLUDES=-I $(TOP)/utils -I $(TOP)/parsing -I $(TOP)/typing -I $(TOP)/bytecomp -I $(TOP)/asmcomp
COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES)

all: 

# To make custom toplevels

install::
#	cp ocamlmktop $(BINDIR)/ocamlmktop

# The preprocessor for asm generators

CVT_EMIT=cvt_emit.cmo

cvt_emit: $(CVT_EMIT)
	$(CAMLC) $(LINKFLAGS) -o cvt_emit $(CVT_EMIT)

clean::
	rm -f cvt_emit

cvt_emit.ml: cvt_emit.mll
	$(CAMLLEX) cvt_emit.mll

clean::
	rm -f cvt_emit.ml

beforedepend:: cvt_emit.ml


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
