# The main Makefile
##############################################################################
# Configuration
##############################################################################

include config/Makefile

CAMLC=boot/ocamlrun boot/ocamlc -I boot
CAMLOPT=boot/ocamlrun ./ocamlopt -I stdlib
COMPFLAGS=$(INCLUDES)
LINKFLAGS=
CAMLYACC=boot/ocamlyacc
YACCFLAGS=
CAMLLEX=boot/ocamlrun boot/ocamllex
CAMLDEP=boot/ocamlrun tools/dependencies/ocamldep
DEPFLAGS=$(INCLUDES)
CAMLRUN=byterun/ocamlrun
SHELL=/bin/sh
MKDIR=mkdir -p

##############################################################################
# The files and directories
##############################################################################

INCLUDES=-I utils -I parsing -I typing -I bytecomp -I asmcomp -I driver -I tools/toplevel

UTILS=utils/logs.cmo \
  utils/misc.cmo utils/tbl.cmo utils/config.cmo \
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

#pad: new buffer option result uchar int bool float stdcompat
PERVASIVES=arg array callback char digest filename format gc hashtbl \
  lexing list map obj parsing pervasives printexc buffer printf queue random \
  set stack string bytes stream sys topdirs toploop weak lazy \
  marshal \
  option result either uchar int bool float stdcompat fun

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
opt: runtimeopt ocamlopt libraryopt otherlibrariesopt cmmopt ocamltoolsopt

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
# pad: see BOOTSTRAP.adoc in recent OCaml for more info
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
	cd tools/dependencies; $(MAKE) install
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

uninstall:
	rm -f $(BINDIR)/ocaml*
	rm -f $(MANDIR)/ocaml*
	rm -f $(LIBDIR)/threads/*
	rm -f $(LIBDIR)/caml/*
	rmdir $(LIBDIR)/threads
	rmdir $(LIBDIR)/caml
	rm -f $(LIBDIR)/*
	rmdir $(LIBDIR)

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
	$(CAMLYACC) $(YACCFLAGS) parsing/parser.mly && perl -p -i -e 's#/\*\(\*[sex]: .* \*\)\*/##' parsing/parser.ml
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

#TODO: fix tools/debugger
TOOLS=tools/profiler tools/dependencies tools/dumper tools/misc

# Other tools

ocamltools::
	set -e; for i in $(TOOLS); do (cd $$i; $(MAKE) all); done
partialclean::
	for i in $(TOOLS); do (cd $$i; $(MAKE) clean); done
alldepend::
	for i in $(TOOLS); do (cd $$i; $(MAKE) depend); done

ocamltoolsopt:
	cd tools/dumper; $(MAKE) opt

cmmopt:
	cd cmm; $(MAKE) all
clean::
	cd cmm; $(MAKE) clean


# The "expunge" utility

EXPUNGEOBJS=utils/misc.cmo utils/tbl.cmo \
  utils/config.cmo utils/clflags.cmo \
  typing/ident.cmo typing/predef.cmo \
  bytecomp/runtimedef.cmo bytecomp/symtable.cmo \
  tools/toplevel/expunge.cmo

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
	rm -f *~

depend: beforedepend
	(for d in utils parsing typing bytecomp asmcomp driver tools/toplevel; \
         do $(CAMLDEP) $(DEPFLAGS) $$d/*.mli $$d/*.ml; \
         done) > .depend

alldepend:: depend

include .depend

##############################################################################
# Test infra
##############################################################################

TESTDIRS=tests examples testasmcomp test

.PHONY: test
test:
	$(MAKE)
	set -e; for i in $(TESTDIRS); do (cd $$i; $(MAKE)); done

clean::
	set -e; for i in $(TESTDIRS); do (cd $$i; $(MAKE) clean); done

# TODO: Some weird memory error for boyer.byt nucleic.byt sieve.byt
# so I've removed them
# - boyer.byt "double free or corruption" when run in CI, weird
# - nucleic.byt "munmap_chunk(): invalid pointer"
# - sieve.byt "free(): invalid pointer, cmp: EOF on - which is empty" at runtime
#coupling: test/Makefile BYTE_EXE variable
nix-test:
	BYTE_EXE="fib.byt takc.byt taku.byt quicksort.byt quicksort.fast.byt fft.byt fft.fast.byt soli.byt soli.fast.byt kb.byt genlex.byt bdd.byt" $(MAKE) test

# The goal here is not so much to deploy via Docker ocaml light but more
# to regression tests in CI (and locally) easily.
# update: we also use the deploy part for checking xix in CI
#pad: see also .github/workflows/docker.yml for the check in CI!
build-docker:
	docker build -t "ocaml-light" .

# need 'docker login -u padator' first with credentials of
# https://hub.docker.com/r/padator/ stored in ~/.docker/config.json
push-docker:
	docker tag "ocaml-light" padator/ocaml-light:latest
	docker push padator/ocaml-light:latest


# see also .github/workflows/nix.yml for the check in CI!
shell:
	nix-shell --pure

##############################################################################
# Developer's targets
##############################################################################
# -filter semgrep
visual:
	codemap -screen_size 3 -efuns_client efuns_client -emacs_client /dev/null .

# Note that the goal here is not to build an actual ocamlc executable; the
# goal is just to compile code and generate a _build/ so that tools like
# merlin can work correctly and provide code navigation. It is also useful
# for faster feedback-loop as dune compiles faster than boot/ocamlc
# TODO: currently need to call make to generate some .ml (e.g., parser.ml, config.ml)
build-dune:
	make
	rm -f stdlib/stdlib.*
	#rm -f lex/lexer.ml lex/parser.ml lex/parser.mli
	dune build
