TOP=../..

include $(TOP)/config/Makefile

CAMLRUN=$(TOP)/boot/ocamlrun
CAMLC=$(CAMLRUN) $(TOP)/boot/ocamlc -I $(TOP)/boot
CAMLLEX=$(CAMLRUN) $(TOP)/boot/ocamllex
INCLUDES=-I $(TOP)/utils -I $(TOP)/parsing -I $(TOP)/typing -I $(TOP)/bytecomp -I $(TOP)/asmcomp
COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES)

all: ocamlprof

# The profiler

CSLPROF=ocamlprof.cmo
CSLPROF_IMPORTS=misc.cmo config.cmo clflags.cmo terminfo.cmo \
  linenum.cmo location.cmo longident.cmo \
  syntaxerr.cmo parser.cmo lexer.cmo parse.cmo

ocamlprof: $(CSLPROF) profiling.cmo
	$(CAMLC) $(LINKFLAGS) -o ocamlprof $(CSLPROF_IMPORTS) $(CSLPROF)

install::
	cp ocamlprof $(BINDIR)/ocamlprof
	cp ocamlcp $(BINDIR)/ocamlcp
	cp profiling.cmi profiling.cmo $(LIBDIR)

clean::
	rm -f ocamlprof


# Common stuff

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

clean::
	rm -f *.cmo *.cmi

beforedepend:

depend: beforedepend
	$(CAMLRUN) ../dependencies/ocamldep $(INCLUDES) *.mli *.ml > .depend

include .depend
