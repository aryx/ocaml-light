# Makefile for the dynamic link library

include ../../config/Makefile

CAMLC=../../boot/ocamlrun ../../boot/ocamlc
INCLUDES=-I ../../utils -I ../../typing -I ../../bytecomp
COMPFLAGS=-I ../../stdlib $(INCLUDES)

OBJS=dynlink.cmo
COMPILEROBJS=misc.cmo config.cmo tbl.cmo clflags.cmo ident.cmo \
  ctype.cmo predef.cmo runtimedef.cmo symtable.cmo opcodes.cmo

all: dynlink.cma extract_crc

allopt:

dynlink.cma: $(OBJS)
	$(CAMLC) $(COMPFLAGS) -a -o dynlink.cma $(COMPILEROBJS) $(OBJS)

extract_crc: dynlink.cma extract_crc.cmo
	$(CAMLC) $(COMPFLAGS) -o extract_crc dynlink.cma extract_crc.cmo

install:
	cp dynlink.cmi dynlink.cma extract_crc $(LIBDIR)

installopt:

partialclean:
	rm -f extract_crc *.cm[ioa]

clean: partialclean

.SUFFIXES: .ml .mli .cmo .cmi

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

depend:
	../../tools/ocamldep $(INCLUDES) *.mli *.ml > .depend

include .depend
