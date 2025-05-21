# Makefile for the str library

include ../../config/Makefile

# Compilation options
CC=$(BYTECC)
CFLAGS=-O  -I../../byterun $(BYTECCCOMPOPTS)
CAMLC=../../boot/ocamlrun ../../boot/ocamlc -I ../../stdlib
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib
COBJS=strstubs.o

all: libstr.a str.cmi str.cma

#pad: see otherlibs/unix/Makefile for this libstropt.a hack
allopt: libstr.a str.cmi str.cmxa libstropt.a

libstr.a: $(COBJS)
	rm -f libstr.a
	ar rc libstr.a $(COBJS)
	$(RANLIB) libstr.a

OPTOBJS=$(COBJS:.o=.ox)
libstropt.a: $(OPTOBJS)
	rm -f libstropt.a
	ar rc libstropt.a $(OPTOBJS)
	$(RANLIB) libstropt.a
.SUFFIXES: .ox
.c.ox:
	$(NATIVECC) -I../../byterun $(NATIVECCCOMPOPTS) -c $< -o $@

str.cma: str.cmo
	$(CAMLC) -a -o str.cma str.cmo

str.cmxa: str.cmx
	$(CAMLOPT) -a -o str.cmxa str.cmx

str.cmx: ../../ocamlopt

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.o *.ox

install:
	cp libstr.a $(LIBDIR)/libstr.a
	cd $(LIBDIR); $(RANLIB) libstr.a
	cp str.cma str.cmi str.mli $(LIBDIR)

installopt:
	cp str.cmx str.cmxa str.a libstropt.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) str.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../tools/ocamldep *.mli *.ml >> .depend

include .depend
