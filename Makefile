# The main Makefile
##############################################################################
# Configuration
##############################################################################

include config/Makefile

CAMLC=boot/ocamlrun boot/ocamlc -I boot
#CAMLC=/home/pad/.opam/4.01.0/bin/ocamlc -bin-annot -I boot
CAMLOPT=boot/ocamlrun ./ocamlopt -I stdlib
COMPFLAGS=$(INCLUDES)
LINKFLAGS=
CAMLYACC=boot/ocamlyacc
YACCFLAGS=
CAMLLEX=boot/ocamlrun boot/ocamllex
CAMLDEP=boot/ocamlrun tools/misc/ocamldep
DEPFLAGS=$(INCLUDES)
CAMLRUN=byterun/ocamlrun
SHELL=/bin/sh
MKDIR=mkdir -p

##############################################################################
# The files and directories
##############################################################################

INCLUDES=-I utils -I parsing -I typing -I bytecomp -I asmcomp -I driver -I tools/toplevel

UTILS=utils/misc.cmo utils/tbl.cmo utils/config.cmo \
  utils/clflags.cmo utils/terminfo.cmo utils/ccomp.cmo

OPTUTILS=$(UTILS) utils/nativeint.cmo

PARSING=parsing/linenum.cmo parsing/location.cmo parsing/longident.cmo \
  parsing/syntaxerr.cmo parsing/parser.cmo \
  parsing/lexer.cmo parsing/parse.cmo

TYPING=typing/ident.cmo typing/path.cmo \
  typing/primitive.cmo typing/types.cmo \
  typing/subst.cmo typing/predef.cmo \
  typing/datarepr.cmo typing/env.cmo \
  typing/ctype.cmo \
  typing/typedtree.cmo typing/printtyp.cmo \
  typing/mtype.cmo typing/includecore.cmo \
  typing/includemod.cmo typing/parmatch.cmo \
  typing/typetexp.cmo typing/typecore.cmo \
  typing/typedecl.cmo  \
  typing/typemod.cmo \

COMP=bytecomp/lambda.cmo bytecomp/printlambda.cmo \
  bytecomp/matching.cmo bytecomp/translcore.cmo \
  bytecomp/translmod.cmo \
  bytecomp/simplif.cmo bytecomp/runtimedef.cmo

BYTECOMP=bytecomp/meta.cmo bytecomp/instruct.cmo bytecomp/bytegen.cmo \
  bytecomp/printinstr.cmo bytecomp/opcodes.cmo bytecomp/emitcode.cmo \
  bytecomp/symtable.cmo bytecomp/bytelibrarian.cmo bytecomp/bytelink.cmo

ASMCOMP=asmcomp/arch.cmo asmcomp/cmm.cmo asmcomp/printcmm.cmo \
  asmcomp/reg.cmo asmcomp/mach.cmo asmcomp/proc.cmo \
  asmcomp/clambda.cmo asmcomp/compilenv.cmo \
  asmcomp/closure.cmo asmcomp/cmmgen.cmo \
  asmcomp/printmach.cmo asmcomp/selectgen.cmo asmcomp/selection.cmo \
  asmcomp/liveness.cmo asmcomp/spill.cmo asmcomp/split.cmo \
  asmcomp/interf.cmo asmcomp/coloring.cmo \
  asmcomp/reloadgen.cmo asmcomp/reload.cmo \
  asmcomp/printlinear.cmo asmcomp/linearize.cmo \
  asmcomp/schedgen.cmo asmcomp/scheduling.cmo \
  asmcomp/emitaux.cmo asmcomp/emit.cmo asmcomp/asmgen.cmo \
  asmcomp/asmlink.cmo asmcomp/asmlibrarian.cmo

DRIVER=driver/errors.cmo driver/compile.cmo driver/main.cmo
OPTDRIVER=driver/opterrors.cmo driver/optcompile.cmo driver/optmain.cmo

TOPLEVEL=driver/errors.cmo driver/compile.cmo \
  tools/toplevel/genprintval.cmo \
  tools/toplevel/printval.cmo tools/toplevel/toploop.cmo \
  tools/toplevel/trace.cmo tools/toplevel/topdirs.cmo

TOPLEVELMAIN=tools/toplevel/topmain.cmo

COMPOBJS=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(DRIVER)

TOPLIB=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(TOPLEVEL)

TOPOBJS=$(TOPLIB) $(TOPLEVELMAIN)

OPTOBJS=$(OPTUTILS) $(PARSING) $(TYPING) $(COMP) $(ASMCOMP) $(OPTDRIVER)

EXPUNGEOBJS=utils/misc.cmo utils/tbl.cmo \
  utils/config.cmo utils/clflags.cmo \
  typing/ident.cmo typing/predef.cmo \
  bytecomp/runtimedef.cmo bytecomp/symtable.cmo \
  tools/toplevel/expunge.cmo

PERVASIVES=arg array callback char digest filename format gc hashtbl \
  lexing list map obj parsing pervasives printexc printf queue random \
  set sort stack string stream sys genlex topdirs toploop weak lazy \
  marshal \
  buffer

##############################################################################
# Main rule
##############################################################################

# Recompile the system using the bootstrap compiler
all: runtime ocamlc ocamllex ocamlyacc ocamltools library ocaml \
  otherlibraries 
#$(DEBUGGER)

# The compilation of ocaml will fail if the runtime has changed.
# Never mind, just do make bootstrap to reach fixpoint again.

# Compile everything the first time
world: coldstart all

# Compile the native-code compiler
opt: runtimeopt ocamlopt libraryopt otherlibrariesopt

clean:: partialclean

##############################################################################
# Coldstart
##############################################################################

LIBFILES=stdlib.cma std_exit.cmo *.cmi camlheader

# Start up the system from the distribution compiler
coldstart:
	cd byterun; $(MAKE) all
	cp byterun/ocamlrun boot/ocamlrun
	cd yacc; $(MAKE) all
	cp yacc/ocamlyacc boot/ocamlyacc
	cd stdlib; $(MAKE) COMPILER=../boot/ocamlc all
	cd stdlib; cp $(LIBFILES) ../boot
	if test -f boot/libcamlrun.a; then :; else \
          ln -s ../byterun/libcamlrun.a boot/libcamlrun.a; fi

##############################################################################
# Bootstrapping
##############################################################################

# Complete bootstrapping cycle
bootstrap:
# Save the original bootstrap compiler
	$(MAKE) backup
# Promote the new compiler but keep the old runtime
# This compiler runs on boot/ocamlrun and produces bytecode for
# byterun/ocamlrun
	$(MAKE) promote-cross
# Rebuild ocamlc and ocamllex (run on byterun/ocamlrun)
	$(MAKE) partialclean
	$(MAKE) ocamlc ocamllex
# Rebuild the library (using byterun/ocamlrun ./ocamlc)
	$(MAKE) library-cross
# Promote the new compiler and the new runtime
	$(MAKE) promote
# Rebuild everything, including ocaml and the tools
	$(MAKE) partialclean
	$(MAKE) all
# Check if fixpoint reached
	$(MAKE) compare


# Save the current bootstrap compiler
MAXSAVED=boot/Saved/Saved.prev/Saved.prev/Saved.prev/Saved.prev/Saved.prev
backup:
	if test -d boot/Saved; then : ; else mkdir boot/Saved; fi
	if test -d $(MAXSAVED); then rm -r $(MAXSAVED); else : ; fi
	mv boot/Saved boot/Saved.prev
	mkdir boot/Saved
	mv boot/Saved.prev boot/Saved/Saved.prev
	cp boot/ocamlrun boot/Saved
	mv boot/ocamlc boot/ocamllex boot/ocamlyacc boot/Saved
	cd boot; cp $(LIBFILES) Saved

# Promote the newly compiled system to the rank of cross compiler
# (Runs on the old runtime, produces code for the new runtime)
promote-cross:
	cp ocamlc boot/ocamlc
	cp lex/ocamllex boot/ocamllex
	cp yacc/ocamlyacc boot/ocamlyacc
	cd stdlib; cp $(LIBFILES) ../boot

# Promote the newly compiled system to the rank of bootstrap compiler
# (Runs on the new runtime, produces code for the new runtime)
promote: promote-cross
	cp byterun/ocamlrun boot/ocamlrun

# Restore the saved bootstrap compiler if a problem arises
restore:
	mv boot/Saved/* boot
	rmdir boot/Saved
	mv boot/Saved.prev boot/Saved

# Check if fixpoint reached
compare:
	@if cmp boot/ocamlc ocamlc && cmp boot/ocamllex lex/ocamllex; \
	then echo "Fixpoint reached, bootstrap succeeded."; \
        else echo "Fixpoint not reached, try one more bootstrapping cycle."; \
	fi

# Remove old bootstrap compilers
cleanboot:
	rm -rf boot/Saved/Saved.prev/*


##############################################################################
# Installation
##############################################################################

install:
	if test -d $(BINDIR); then : ; else $(MKDIR) $(BINDIR); fi
	if test -d $(LIBDIR); then : ; else $(MKDIR) $(LIBDIR); fi
	if test -d $(MANDIR); then : ; else $(MKDIR) $(MANDIR); fi
	cd byterun; $(MAKE) install
	cp ocamlc $(BINDIR)/ocamlc
	cp ocaml $(BINDIR)/ocaml
	cd stdlib; $(MAKE) install
	cp lex/ocamllex $(BINDIR)/ocamllex
	cp yacc/ocamlyacc $(BINDIR)/ocamlyacc
	$(CAMLC) -a -o $(LIBDIR)/toplevellib.cma $(TOPLIB)
	cp expunge $(LIBDIR)
	cp tools/toplevel/topmain.cmo $(LIBDIR)
	cp tools/toplevel/toploop.cmi tools/toplevel/topdirs.cmi $(LIBDIR)
	cd tools/misc; $(MAKE) install
	cd docs/man; for i in *.m; do cp $$i $(MANDIR)/`basename $$i .m`.$(MANEXT); done
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) install); done
	if test -f ocamlopt; then $(MAKE) installopt; else :; fi
	if test -f tools/debugger/ocamldebug; then (cd tools/debugger; $(MAKE) install); else :; fi

# Installation of the native-code compiler
installopt:
	cd asmrun; $(MAKE) install
	cp ocamlopt $(BINDIR)/ocamlopt
	cd stdlib; $(MAKE) installopt
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) installopt); done
	if test -f ocamlc.opt; then cp ocamlc.opt $(BINDIR)/ocamlc.opt; else :; fi
	if test -f ocamlopt.opt; then cp ocamlopt.opt $(BINDIR)/ocamlopt.opt; else :; fi


##############################################################################
# The binaries
##############################################################################

# The compiler

ocamlc: $(COMPOBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamlc $(COMPOBJS)

partialclean::
	rm -f ocamlc

# The native-code compiler

ocamlopt: $(OPTOBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamlopt $(OPTOBJS)

partialclean::
	rm -f ocamlopt

# The toplevel

ocaml: $(TOPOBJS) expunge
	$(CAMLC) $(LINKFLAGS) -linkall -o ocaml.tmp $(TOPOBJS)
	- $(CAMLRUN) ./expunge ocaml.tmp ocaml $(PERVASIVES)
	rm -f ocaml.tmp

partialclean::
	rm -f ocaml

# The bytecode compiler compiled with the native-code compiler

ocamlc.opt: $(COMPOBJS:.cmo=.cmx)
	cd asmrun; $(MAKE) meta.o
	$(CAMLOPT) $(LINKFLAGS) -o ocamlc.opt $(COMPOBJS:.cmo=.cmx) asmrun/meta.o

partialclean::
	rm -f ocamlc.opt

# The native-code compiler compiled with itself

ocamlopt.opt: $(OPTOBJS:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) -o ocamlopt.opt $(OPTOBJS:.cmo=.cmx)

partialclean::
	rm -f ocamlopt.opt

$(OPTOBJS:.cmo=.cmx): ocamlopt

##############################################################################
# The configuration file
##############################################################################

utils/config.ml: utils/config.mlp config/Makefile
	@rm -f utils/config.ml
	sed -e 's|%%LIBDIR%%|$(LIBDIR)|' \
            -e 's|%%BYTECC%%|$(BYTECC) $(BYTECCLINKOPTS)|' \
            -e 's|%%NATIVECC%%|$(NATIVECC) $(NATIVECCLINKOPTS)|' \
            -e 's|%%PARTIALLD%%|ld -r $(NATIVECCLINKOPTS)|' \
            -e 's|%%CCLIBS%%|$(CCLIBS)|' \
            -e 's|%%RANLIBCMD%%|$(RANLIBCMD)|' \
            -e 's|%%ARCH%%|$(ARCH)|' \
            -e 's|%%MODEL%%|$(MODEL)|' \
            -e 's|%%SYSTEM%%|$(SYSTEM)|' \
            -e 's|%%EXT_OBJ%%|.o|' \
            -e 's|%%EXT_ASM%%|.s|' \
            -e 's|%%EXT_LIB%%|.a|' \
            utils/config.mlp > utils/config.ml
	@chmod -w utils/config.ml

partialclean::
	rm -f utils/config.ml

beforedepend:: utils/config.ml

##############################################################################
# Generated code (lex & yacc files)
##############################################################################

# The parser

parsing/parser.mli parsing/parser.ml: parsing/parser.mly
	$(CAMLYACC) $(YACCFLAGS) parsing/parser.mly
	perl -p -i -e 's#/\*\(\*[sex]: .* \*\)\*/##' parsing/parser.ml
#pad: this  perl thing is just because ocamlyacc does not handle well
# syncweb special comment at the very end

partialclean::
	rm -f parsing/parser.mli parsing/parser.ml parsing/parser.output

beforedepend:: parsing/parser.mli parsing/parser.ml

# The lexer

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) parsing/lexer.mll

partialclean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# The auxiliary lexer for counting line numbers

parsing/linenum.ml: parsing/linenum.mll
	$(CAMLLEX) parsing/linenum.mll

partialclean::
	rm -f parsing/linenum.ml

beforedepend:: parsing/linenum.ml


##############################################################################
# Generated code (other)
##############################################################################

# The numeric opcodes

bytecomp/opcodes.ml: byterun/instruct.h
	cat byterun/instruct.h | grep -v '/' | \
	sed -n -e '/^enum/p' -e 's/,//g' -e '/^  /p' | \
        awk -f tools/misc/make-opcodes > bytecomp/opcodes.ml

partialclean::
	rm -f bytecomp/opcodes.ml

beforedepend:: bytecomp/opcodes.ml

# The predefined exceptions and primitives

byterun/primitives:
	cd byterun; $(MAKE) primitives

bytecomp/runtimedef.ml: byterun/primitives byterun/fail.h
	(echo 'let builtin_exceptions = [|'; \
	 sed -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$$|  \1;|p' byterun/fail.h | \
	 sed -e '$$s/;$$//'; \
         echo '|]'; \
         echo 'let builtin_primitives = [|'; \
         sed -e 's/.*/  "&";/' -e '$$s/;$$//' byterun/primitives; \
	 echo '|]') > bytecomp/runtimedef.ml

partialclean::
	rm -f bytecomp/runtimedef.ml

beforedepend:: bytecomp/runtimedef.ml

# Choose the right machine-dependent files

asmcomp/arch.ml: asmcomp/$(ARCH)/arch.ml
	ln -s $(ARCH)/arch.ml asmcomp/arch.ml

partialclean::
	rm -f asmcomp/arch.ml

beforedepend:: asmcomp/arch.ml

asmcomp/proc.ml: asmcomp/$(ARCH)/proc.ml
	ln -s $(ARCH)/proc.ml asmcomp/proc.ml

partialclean::
	rm -f asmcomp/proc.ml

beforedepend:: asmcomp/proc.ml

asmcomp/selection.ml: asmcomp/$(ARCH)/selection.ml
	ln -s $(ARCH)/selection.ml asmcomp/selection.ml

partialclean::
	rm -f asmcomp/selection.ml

beforedepend:: asmcomp/selection.ml

asmcomp/reload.ml: asmcomp/$(ARCH)/reload.ml
	ln -s $(ARCH)/reload.ml asmcomp/reload.ml

partialclean::
	rm -f asmcomp/reload.ml

beforedepend:: asmcomp/reload.ml

asmcomp/scheduling.ml: asmcomp/$(ARCH)/scheduling.ml
	ln -s $(ARCH)/scheduling.ml asmcomp/scheduling.ml

partialclean::
	rm -f asmcomp/scheduling.ml

beforedepend:: asmcomp/scheduling.ml

# Preprocess the code emitters

asmcomp/emit.ml: asmcomp/$(ARCH)/emit.mlp tools/misc/cvt_emit
	boot/ocamlrun tools/misc/cvt_emit < asmcomp/$(ARCH)/emit.mlp > asmcomp/emit.ml \
        || { rm -f asmcomp/emit.ml; exit 2; }

partialclean::
	rm -f asmcomp/emit.ml

beforedepend:: asmcomp/emit.ml

tools/misc/cvt_emit: tools/misc/cvt_emit.mll
	cd tools/misc; $(MAKE) cvt_emit


partialclean::
	rm -f asmcomp/emit.ml

beforedepend:: asmcomp/emit.ml


##############################################################################
# Runtimes
##############################################################################

# The runtime system for the bytecode compiler

runtime:
	cd byterun; $(MAKE) all
	if test -f stdlib/libcamlrun.a; then :; else \
          ln -s ../byterun/libcamlrun.a stdlib/libcamlrun.a; fi
clean::
	cd byterun; $(MAKE) clean
	rm -f stdlib/libcamlrun.a
alldepend::
	cd byterun; $(MAKE) depend

# The runtime system for the native-code compiler

runtimeopt:
	cd asmrun; $(MAKE) all
	if test -f stdlib/libasmrun.a; then :; else \
          ln -s ../asmrun/libasmrun.a stdlib/libasmrun.a; fi
clean::
	cd asmrun; $(MAKE) clean
	rm -f stdlib/libasmrun.a
alldepend::
	cd asmrun; $(MAKE) depend

##############################################################################
# The library
##############################################################################

library:
	cd stdlib; $(MAKE) all
library-cross:
	cd stdlib; $(MAKE) RUNTIME=../byterun/ocamlrun all
libraryopt:
	cd stdlib; $(MAKE) allopt
partialclean::
	cd stdlib; $(MAKE) clean
alldepend::
	cd stdlib; $(MAKE) depend

##############################################################################
# The extra libraries
##############################################################################

otherlibraries:
	set -e; for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) all); done
otherlibrariesopt:
	set -e; for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) allopt); done
partialclean::
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) partialclean); done
clean::
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) clean); done
alldepend::
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) depend); done


##############################################################################
# Tools
##############################################################################

ocamllex:
	cd lex; $(MAKE) all
partialclean::
	cd lex; $(MAKE) clean
alldepend::
	cd lex; $(MAKE) depend

ocamlyacc:
	cd yacc; $(MAKE) all
clean::
	cd yacc; $(MAKE) clean

# Tools

ocamltools:
	cd tools/misc; $(MAKE) all
partialclean::
	cd tools/misc; $(MAKE) clean
alldepend::
	cd tools/misc; $(MAKE) depend

# The replay debugger

ocamldebugger:
	cd tools/debugger; $(MAKE) all
partialclean::
	cd tools/debugger; $(MAKE) clean
alldepend::
	cd tools/debugger; $(MAKE) depend


# The "expunge" utility

expunge: $(EXPUNGEOBJS)
	$(CAMLC) $(LINKFLAGS) -o expunge $(EXPUNGEOBJS)

partialclean::
	rm -f expunge

##############################################################################
# Default rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

partialclean::
	rm -f utils/*.cm[iox] utils/*.[so] utils/*~
	rm -f parsing/*.cm[iox] parsing/*.[so] parsing/*~
	rm -f typing/*.cm[iox] typing/*.[so] typing/*~
	rm -f bytecomp/*.cm[iox] bytecomp/*.[so] bytecomp/*~
	rm -f asmcomp/*.cm[iox] asmcomp/*.[so] asmcomp/*~
	rm -f driver/*.cm[iox] driver/*.[so] driver/*~
	rm -f tools/toplevel/*.cm[iox] tools/toplevel/*.[so] tools/toplevel/*~
	rm -f tools/misc/*.cm[iox] tools/misc/*.[so] tools/misc/*~
	rm -f *~

depend: beforedepend
	(for d in utils parsing typing bytecomp asmcomp driver tools/toplevel; \
         do $(CAMLDEP) $(DEPFLAGS) $$d/*.mli $$d/*.ml; \
         done) > .depend

alldepend:: depend

include .depend

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

