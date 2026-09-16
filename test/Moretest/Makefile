include ../../config/Makefile

CAMLC=../../boot/ocamlrun ../../ocamlc -I ../../stdlib
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib
OPTFLAGS=-S
CAMLDEP=../../boot/ocamlrun ../../tools/ocamldep
CAMLRUN=../../byterun/ocamlrun
CODERUNPARAMS=CAMLRUNPARAM='o=100'

callback.byt: callback.cmo callbackprim.o
	$(CAMLC) -o callback.byt -custom callback.cmo callbackprim.o ../../otherlibs/unix/libunix.a
callback.out: callback.cmx callbackprim.o
	$(CAMLOPT) -o callback.out callback.cmx callbackprim.o ../../otherlibs/unix/libunix.a

manyargs.byt: manyargs.cmo manyargsprim.o
	$(CAMLC) -o manyargs.byt -custom manyargs.cmo manyargsprim.o
manyargs.out: manyargs.cmx manyargsprim.o
	$(CAMLOPT) -o manyargs.out manyargs.cmx manyargsprim.o

multdef.out: multdef.cmx usemultdef.cmx
	$(CAMLOPT) -o multdef.out multdef.cmx usemultdef.cmx

cm.byt: cmcaml.ml cmstub.c cmmain.c
	$(CAMLC) -custom -o cm.byt cmcaml.ml cmstub.c cmmain.c

cmlinked.out: cmcaml.ml cmstub.c cmmain.c
	$(CAMLC) -output-obj -o cm.o cmcaml.ml
	$(BYTECC) -g -o cmlinked.out cm.o -I../../byterun -DNO_BYTECODE_FILE cmstub.c cmmain.c ../../byterun/libcamlrun.a $(CCLIBS)

cm.out: cmcaml.ml cmstub.c cmmain.c
	$(CAMLOPT) -output-obj -o cm.o cmcaml.ml
	$(NATIVECC) -g -o cm.out cm.o -I../../byterun -DNO_BYTECODE_FILE cmstub.c cmmain.c ../../asmrun/libasmrun.a $(CCLIBS)

# Common rules

.SUFFIXES:
.SUFFIXES: .mli .ml .cmi .cmo .cmx .byt .fast.byt .out .fast.out .c .o

.ml.byt:
	$(CAMLC) -o $*.byt $<

.ml.fast.byt:
	cp $*.ml $*_fast.ml
	$(CAMLC) -unsafe -o $*.fast.byt $*_fast.ml
	rm -f $*_fast.ml

.ml.out:
	$(CAMLOPT) $(OPTFLAGS) -o $*.out $<

.ml.fast.out:
	cp $*.ml $*_fast.ml
	$(CAMLOPT) $(OPTFLAGS) -unsafe -o $*.fast.out $*_fast.ml
	rm -f $*_fast.ml

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmo:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) $(OPTFLAGS) -c $<

.c.o:
	$(NATIVECC) $(NATIVECCCOMPOPTS) -I../../byterun -c $<

clean::
	rm -f *.byt *.out
	rm -f *.cm[iox] *.[os]
	rm -f *~
	rm -f intext.data

# claude: light regression-test wiring for these pre-existing (upstream
# OCaml 1.07) test programs, added 2026 alongside the let*/let+ backport
# work (this directory was never previously wired into `make test` at
# all). Not everything here is included:
#  - sets.ml uses Set.Make (a functor) -- ocaml-light has no functors at
#    all (see CLAUDE.md), so this test is fundamentally inapplicable, not
#    just broken.
#  - signals.ml is interactive (expects a human to press ctrl-C/ctrl-Z
#    and type input at the terminal) -- not automatable without a proper
#    expect-style driver.
#  - syserror.ml deliberately crashes with an uncaught Sys_error whose
#    exact text is libc/errno-message-dependent -- too fragile to pin
#    down with an exact-match Results/*.out across platforms.
#  - TODO: manyargs.ml, tcallback.ml, cmcaml.ml need their C stubs
#    (manyargsprim.c, callbackprim.c, cmstub.c/cmmain.c) wired in too;
#    tcallback's own historical Makefile rule above is itself stale
#    (references a nonexistent callback.cmo instead of tcallback.cmo).
#    Skipped for now, not attempted.

MORETEST_PROGS=arrays bigints equality intext io letstar patmatch \
  recvalues testrandom wc

multdef.byt: multdef.cmo usemultdef.cmo
	$(CAMLC) -o multdef.byt multdef.cmo usemultdef.cmo

moretest_all: $(MORETEST_PROGS:=.byt) multdef.byt

moretest_test: moretest_all
	set -e; \
	for prog in $(MORETEST_PROGS); do \
	  echo $$prog; \
	  case $$prog in \
	    io) $(CAMLRUN) io.byt io.ml 2>&1 | cmp - Results/io.out;; \
	    wc) $(CAMLRUN) wc.byt < wc.ml 2>&1 | cmp - Results/wc.out;; \
	    *) $(CAMLRUN) $$prog.byt 2>&1 | cmp - Results/$$prog.out;; \
	  esac; \
	done
	echo multdef
	$(CAMLRUN) multdef.byt 2>&1 | cmp - Results/multdef.out

clean::
	rm -f multdef.byt

# Dependencies

depend:
	$(CAMLDEP) *.mli *.ml > .depend

include .depend

