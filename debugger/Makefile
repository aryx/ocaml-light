TOP=../

include $(TOP)/config/Makefile

CAMLC=$(TOP)/boot/ocamlrun $(TOP)/ocamlc -I $(TOP)/boot
COMPFLAGS=$(INCLUDES)
LINKFLAGS=-custom -linkall
CAMLYACC=$(TOP)/boot/ocamlyacc
YACCFLAGS=
CAMLLEX=$(TOP)/boot/ocamlrun $(TOP)/boot/ocamllex
CAMLDEP=$(TOP)/boot/ocamlrun $(TOP)/tools/ocamldep
DEPFLAGS=$(INCLUDES)

INCLUDES=\
  -I $(TOP)/utils -I $(TOP)/parsing -I $(TOP)/typing -I $(TOP)/bytecomp -I $(TOP)/toplevel \
  -I $(TOP)/otherlibs/unix -I $(TOP)/otherlibs/dynlink

OTHEROBJS=\
  $(TOP)/otherlibs/unix/unix.cma \
  $(TOP)/utils/misc.cmo $(TOP)/utils/config.cmo \
  $(TOP)/utils/tbl.cmo $(TOP)/utils/clflags.cmo \
  $(TOP)/parsing/longident.cmo \
  $(TOP)/typing/ident.cmo $(TOP)/typing/path.cmo \
  $(TOP)/typing/btype.cmo $(TOP)/typing/primitive.cmo $(TOP)/typing/typedtree.cmo \
  $(TOP)/typing/subst.cmo $(TOP)/typing/predef.cmo \
  $(TOP)/typing/datarepr.cmo $(TOP)/typing/env.cmo \
  $(TOP)/typing/ctype.cmo $(TOP)/typing/printtyp.cmo $(TOP)/typing/mtype.cmo \
  $(TOP)/bytecomp/runtimedef.cmo $(TOP)/bytecomp/symtable.cmo \
  $(TOP)/bytecomp/opcodes.cmo $(TOP)/toplevel/genprintval.cmo \
  $(TOP)/otherlibs/dynlink/dynlink.cmo

OBJS=\
	primitives.cmo \
	unix_tools.cmo \
	debugger_config.cmo \
	envaux.cmo \
	parameters.cmo \
	lexer.cmo \
	input_handling.cmo \
	debugcom.cmo \
	exec.cmo \
	source.cmo \
	checkpoints.cmo \
	symbols.cmo \
	events.cmo \
	breakpoints.cmo \
	trap_barrier.cmo \
	history.cmo \
	program_loading.cmo \
	printval.cmo \
	show_source.cmo \
	time_travel.cmo \
	program_management.cmo \
	frames.cmo \
	eval.cmo \
	show_information.cmo \
        loadprinter.cmo \
	parser.cmo \
	command_line.cmo \
	main.cmo

all: ocamldebug

ocamldebug: $(OBJS) $(OTHEROBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamldebug $(OTHEROBJS) $(OBJS) $(TOP)/otherlibs/unix/libunix.a

install:
	cp ocamldebug $(BINDIR)/ocamldebug

clean::
	rm -f ocamldebug
	rm -f *.cmo *.cmi

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

depend: beforedepend
	$(CAMLDEP) $(DEPFLAGS) *.mli *.ml > .depend

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll
clean::
	rm -f lexer.ml
beforedepend:: lexer.ml

parser.ml parser.mli: parser.mly
	$(CAMLYACC) parser.mly
clean::
	rm -f parser.ml parser.mli
beforedepend:: parser.ml parser.mli

include .depend
