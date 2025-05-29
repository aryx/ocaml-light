TOP=..

include $(TOP)/config/Makefile

CAMLC=$(TOP)/boot/ocamlrun $(TOP)/boot/ocamlc -I $(TOP)/boot
COMPFLAGS=$(INCLUDES)
LINKFLAGS=
CAMLYACC=$(TOP)/boot/ocamlyacc
YACCFLAGS=
CAMLLEX=$(TOP)/boot/ocamlrun $(TOP)/boot/ocamllex
CAMLDEP=$(TOP)/boot/ocamlrun $(TOP)/tools/ocamldep
DEPFLAGS=$(INCLUDES)
CAMLRUN=$(TOP)/boot/ocamlrun

all: codegen

INCLUDES=-I $(TOP)/utils -I $(TOP)/typing -I $(TOP)/asmcomp

#TODO: use lib.a instead of big list of cmos
#TODO: try reduce deps to asmcomp/ files? need all of them? use codegraph to reduce
OTHEROBJS=\
  $(TOP)/utils/misc.cmo $(TOP)/utils/config.cmo $(TOP)/utils/tbl.cmo \
  $(TOP)/utils/nativeint.cmo $(TOP)/utils/clflags.cmo $(TOP)/utils/logs.cmo \
  $(TOP)/utils/ccomp.cmo $(TOP)/utils/config.cmo $(TOP)/utils/clflags.cmo \
  \
  $(TOP)/parsing/linenum.cmo $(TOP)/parsing/location.cmo \
  \
  $(TOP)/typing/ident.cmo $(TOP)/typing/path.cmo $(TOP)/typing/types.cmo \
  $(TOP)/typing/subst.cmo $(TOP)/typing/primitive.cmo \
  $(TOP)/typing/predef.cmo $(TOP)/typing/datarepr.cmo $(TOP)/typing/env.cmo \
  \
  $(TOP)/bytecomp/lambda.cmo \
  \
  $(TOP)/asmcomp/arch.cmo $(TOP)/asmcomp/cmm.cmo $(TOP)/asmcomp/printcmm.cmo \
  $(TOP)/asmcomp/clambda.cmo $(TOP)/asmcomp/compilenv.cmo \
  $(TOP)/asmcomp/reg.cmo $(TOP)/asmcomp/mach.cmo $(TOP)/asmcomp/proc.cmo \
  $(TOP)/asmcomp/closure.cmo $(TOP)/asmcomp/cmmgen.cmo \
  $(TOP)/asmcomp/printmach.cmo \
  $(TOP)/asmcomp/selectgen.cmo $(TOP)/asmcomp/selection.cmo \
  $(TOP)/asmcomp/liveness.cmo $(TOP)/asmcomp/spill.cmo $(TOP)/asmcomp/split.cmo \
  $(TOP)/asmcomp/interf.cmo $(TOP)/asmcomp/coloring.cmo \
  $(TOP)/asmcomp/reloadgen.cmo $(TOP)/asmcomp/reload.cmo \
  $(TOP)/asmcomp/linearize.cmo $(TOP)/asmcomp/schedgen.cmo \
  $(TOP)/asmcomp/scheduling.cmo \
  $(TOP)/asmcomp/printlinear.cmo $(TOP)/asmcomp/emitaux.cmo \
  $(TOP)/asmcomp/emit.cmo $(TOP)/asmcomp/asmgen.cmo

OBJS=parsecmmaux.cmo parsecmm.cmo lexcmm.cmo main.cmo

codegen: $(OTHEROBJS) $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o codegen $(OTHEROBJS) $(OBJS)
clean::
	rm -f codegen

# The parser

parsecmm.mli parsecmm.ml: parsecmm.mly
	$(CAMLYACC) $(YACCFLAGS) parsecmm.mly

clean::
	rm -f parsecmm.mli parsecmm.ml parsecmm.output

beforedepend:: parsecmm.mli parsecmm.ml

# The lexer

lexcmm.ml: lexcmm.mll
	$(CAMLLEX) lexcmm.mll

clean::
	rm -f lexcmm.ml

beforedepend:: lexcmm.ml



.SUFFIXES:
.SUFFIXES: .cmm .c .o .S .ml .mli .cmo .cmi

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

clean::
	rm -f *.out *.cm[io] *.s *.o *~

depend: beforedepend
	$(CAMLDEP) $(DEPFLAGS) *.mli *.ml > .depend

include .depend
