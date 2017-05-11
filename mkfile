##############################################################################
# Developer rules
##############################################################################

# go in stdlib/
# make with recent ocamlc -bin-annot; cp *.cm[ioa] ../boot
# go at the top
# make; make ocamlopt; make ocaml
# => have lots of .cmt now
graph:
	~/pfff/codegraph -derived_data -lang cmt -build .
check:
	~/pfff/scheck -with_graph_code graph_code.marshall -filter 3 . 2>&1 | grep -v stdlib | grep Function

##############################################################################
# Literate Programming rules
##############################################################################
TOP=.

include $(TOP)/docs/latex/Makefile.common

TEXMAIN=OCaml.nw
TEX=OCaml.tex

SRC_ORIG=OCaml.nw OCaml_extra.nw

#ML sources
SRC_VIEWS= \
  ./utils/config.mli\
  ./utils/misc.mli\
  ./utils/misc.ml\
  ./utils/tbl.mli\
  ./utils/tbl.ml\
  ./utils/terminfo.mli\
  ./utils/terminfo.ml\
  ./utils/clflags.ml\
  ./utils/ccomp.mli\
  ./utils/ccomp.ml\
  \
  ./parsing/asttypes.mli\
  ./parsing/linenum.mli\
  ./parsing/longident.mli\
  ./parsing/longident.ml\
  ./parsing/location.mli\
  ./parsing/location.ml\
  ./parsing/syntaxerr.mli\
  ./parsing/syntaxerr.ml\
  ./parsing/parsetree.mli\
  ./parsing/lexer.mli\
  ./parsing/lexer.mll\
  ./parsing/parse.mli\
  ./parsing/parse.ml\
  \
  ./typing/ident.mli\
  ./typing/ident.ml\
  ./typing/primitive.mli\
  ./typing/primitive.ml\
  ./typing/path.mli\
  ./typing/path.ml\
  ./typing/types.ml\
  ./typing/subst.mli\
  ./typing/subst.ml\
  ./typing/predef.mli\
  ./typing/predef.ml\
  ./typing/datarepr.mli\
  ./typing/datarepr.ml\
  ./typing/env.mli\
  ./typing/env.ml\
  ./typing/typedtree.mli\
  ./typing/typedtree.ml\
  ./typing/ctype.mli\
  ./typing/ctype.ml\
  ./typing/includecore.mli\
  ./typing/includecore.ml\
  ./typing/mtype.mli\
  ./typing/mtype.ml\
  ./typing/printtyp.mli\
  ./typing/printtyp.ml\
  ./typing/parmatch.mli\
  ./typing/parmatch.ml\
  ./typing/typetexp.mli\
  ./typing/typetexp.ml\
  ./typing/includemod.mli\
  ./typing/includemod.ml\
  ./typing/typedecl.mli\
  ./typing/typedecl.ml\
  ./typing/typecore.mli\
  ./typing/typecore.ml\
  ./typing/typemod.mli\
  ./typing/typemod.ml\
  \
  ./bytecomp/lambda.mli\
  ./bytecomp/lambda.ml\
  ./bytecomp/printlambda.mli\
  ./bytecomp/printlambda.ml\
  ./bytecomp/meta.mli\
  ./bytecomp/meta.ml\
  ./bytecomp/runtimedef.mli\
  ./bytecomp/instruct.mli\
  ./bytecomp/instruct.ml\
  ./bytecomp/printinstr.mli\
  ./bytecomp/printinstr.ml\
  ./bytecomp/simplif.mli\
  ./bytecomp/simplif.ml\
  ./bytecomp/matching.mli\
  ./bytecomp/matching.ml\
  ./bytecomp/translcore.mli\
  ./bytecomp/translcore.ml\
  ./bytecomp/bytegen.mli\
  ./bytecomp/bytegen.ml\
  ./bytecomp/translmod.mli\
  ./bytecomp/translmod.ml\
  ./bytecomp/emitcode.mli\
  ./bytecomp/emitcode.ml\
  ./bytecomp/bytelibrarian.mli\
  ./bytecomp/bytelibrarian.ml\
  ./bytecomp/symtable.mli\
  ./bytecomp/symtable.ml\
  ./bytecomp/bytelink.mli\
  ./bytecomp/bytelink.ml\
  \
  ./driver/compile.mli\
  ./driver/compile.ml\
  ./driver/errors.mli\
  ./driver/errors.ml\
  ./driver/main.ml\
  \
  asmcomp/arm/arch.ml\
  asmcomp/clambda.mli\
  asmcomp/clambda.ml\
  asmcomp/emitaux.mli\
  asmcomp/emitaux.ml\
  asmcomp/scheduling.mli\
  asmcomp/arm/scheduling.ml\
  asmcomp/cmm.mli\
  asmcomp/cmm.ml\
  asmcomp/printcmm.mli\
  asmcomp/printcmm.ml\
  asmcomp/compilenv.mli\
  asmcomp/compilenv.ml\
  asmcomp/reg.mli\
  asmcomp/reg.ml\
  asmcomp/asmlibrarian.mli\
  asmcomp/asmlibrarian.ml\
  asmcomp/closure.mli\
  asmcomp/closure.ml\
  asmcomp/mach.mli\
  asmcomp/mach.ml\
  asmcomp/printmach.mli\
  asmcomp/printmach.ml\
  asmcomp/split.mli\
  asmcomp/split.ml\
  asmcomp/proc.mli\
  asmcomp/arm/proc.ml\
  asmcomp/reloadgen.mli\
  asmcomp/reloadgen.ml\
  asmcomp/interf.mli\
  asmcomp/reload.mli\
  asmcomp/arm/reload.ml\
  asmcomp/interf.ml\
  asmcomp/coloring.mli\
  asmcomp/coloring.ml\
  asmcomp/linearize.mli\
  asmcomp/linearize.ml\
  asmcomp/printlinear.mli\
  asmcomp/printlinear.ml\
  asmcomp/spill.mli\
  asmcomp/spill.ml\
  asmcomp/selectgen.mli\
  asmcomp/selectgen.ml\
  asmcomp/cmmgen.mli\
  asmcomp/cmmgen.ml\
  asmcomp/liveness.mli\
  asmcomp/liveness.ml\
  asmcomp/schedgen.mli\
  asmcomp/selection.mli\
  asmcomp/arm/selection.ml\
  asmcomp/schedgen.ml\
  asmcomp/emit.mli\
  asmcomp/asmgen.mli\
  asmcomp/asmgen.ml\
  asmcomp/asmlink.mli\
  asmcomp/asmlink.ml\
  \
  ./driver/optcompile.mli\
  ./driver/optcompile.ml\
  ./driver/opterrors.mli\
  ./driver/opterrors.ml\
  ./driver/optmain.ml\

# lex/* is synced with plan9/generator/CompilerGenerator.nw
# as well as stdlib/lexing.ml

sync::
	$(MAKE) sync2
	@echo do not forget also make sync_c

sync_ml:
	$(MAKE) sync

sync2:
	$(MAKE) LANG=ocamlyacc sync3

sync3:
	$(SYNCWEB) $(SRC_ORIG) parsing/parser.mly


SRC_VIEWS_C=\
  byterun/config.h\
  byterun/instruct.h\
  byterun/mlvalues.h\
  byterun/exec.h\
  byterun/prims.h\
  byterun/memory.h\
  byterun/memory.c\
  byterun/freelist.h\
  byterun/freelist.c\
  byterun/alloc.h\
  byterun/alloc.c\
  byterun/interp.h\
  byterun/interp.c\
  byterun/gc.h\
  byterun/gc_ctrl.h\
  byterun/gc_ctrl.c\
  byterun/roots.h\
  byterun/roots.c\
  byterun/major_gc.h\
  byterun/major_gc.c\
  byterun/minor_gc.h\
  byterun/minor_gc.c\
  byterun/compact.h\
  byterun/compact.c\
  byterun/hash.c\
  byterun/array.c\
  byterun/stacks.h\
  byterun/stacks.c\
  byterun/callback.h\
  byterun/callback.c\
  byterun/compare.c\
  byterun/debugger.h\
  byterun/debugger.c\
  byterun/instrtrace.h\
  byterun/instrtrace.c\
  byterun/extern.c\
  byterun/fail.h\
  byterun/fail.c\
  byterun/fix_code.h\
  byterun/fix_code.c\
  byterun/intext.h\
  byterun/intern.c\
  byterun/ints.c\
  byterun/floats.c\
  byterun/io.h\
  byterun/io.c\
  byterun/lexing.c\
  byterun/md5.h\
  byterun/md5.c\
  byterun/meta.c\
  byterun/misc.h\
  byterun/misc.c\
  byterun/obj.c\
  byterun/parsing.c\
  byterun/printexc.c\
  byterun/reverse.h\
  byterun/signals.h\
  byterun/signals.c\
  byterun/str.h\
  byterun/str.c\
  byterun/sys.h\
  byterun/sys.c\
  byterun/terminfo.c\
  byterun/weak.h\
  byterun/weak.c\
  byterun/startup.c\
  byterun/main.c\


SYNCWEB_C=~/github/syncweb/syncweb.opt -md5sum_in_auxfile -less_marks -lang C
sync_c:
	for i in $(SRC_VIEWS_C); do echo $$i; $(SYNCWEB_C) $(SRC_ORIG) $$i || exit 1; done 
