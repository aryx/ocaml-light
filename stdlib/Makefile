include ../config/Makefile

RUNTIME=../boot/ocamlrun
COMPILER=../ocamlc
#COMPILER=/home/pad/.opam/4.01.0/bin/ocamlc
CAMLC=$(RUNTIME) $(COMPILER)
COMPFLAGS=-g
#  -bin-annot
OPTCOMPILER=../ocamlopt
CAMLOPT=$(RUNTIME) $(OPTCOMPILER)
OPTCOMPFLAGS=-verbose
CAMLDEP=../boot/ocamlrun ../tools/dependencies/ocamldep

#coupling: modify also ../Makefile PERVASIVES and otherlibs/threads/Makefile
#  and maybe also tools/debugger/debugger_config.ml stdlib_units constants
#pad: now there is an order, parsing depends on stack, printf/printexc depends
# on buffer, etc.
OBJS=pervasives.cmo list.cmo char.cmo string.cmo bytes.cmo array.cmo sys.cmo \
  hashtbl.cmo filename.cmo marshal.cmo obj.cmo \
  set.cmo map.cmo stack.cmo queue.cmo stream.cmo \
  buffer.cmo printf.cmo format.cmo arg.cmo gc.cmo \
  digest.cmo random.cmo callback.cmo weak.cmo \
  lazy.cmo \
  lexing.cmo parsing.cmo \
  printexc.cmo \
  option.cmo result.cmo uchar.cmo int.cmo bool.cmo float.cmo stdcompat.cmo fun.cmo

all: stdlib.cma std_exit.cmo camlheader

allopt: stdlib.cmxa std_exit.cmx

install:
	cp stdlib.cma std_exit.cmo *.cmi *.mli *.ml camlheader $(LIBDIR)

installopt:
	cp stdlib.cmxa stdlib.a std_exit.o *.cmx $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) stdlib.a

stdlib.cma: $(OBJS)
	$(CAMLC) -a -o stdlib.cma $(OBJS)

stdlib.cmxa: $(OBJS:.cmo=.cmx)
	$(CAMLOPT) -a -o stdlib.cmxa $(OBJS:.cmo=.cmx)

camlheader: header.c ../config/Makefile
	if $(SHARPBANGSCRIPTS); \
	then echo "#!$(BINDIR)/ocamlrun" > camlheader; \
	else $(BYTECC) $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) '-DRUNTIME_NAME="$(BINDIR)/ocamlrun"' header.c -o camlheader; \
	     strip camlheader; \
        fi

clean::
	rm -f camlheader

pervasives.cmi: pervasives.mli
	$(CAMLC) $(COMPFLAGS) -nopervasives -c pervasives.mli

pervasives.cmo: pervasives.ml
	$(CAMLC) $(COMPFLAGS) -nopervasives -c pervasives.ml

pervasives.cmx: pervasives.ml
	$(CAMLOPT) $(OPTCOMPFLAGS) -nopervasives -c pervasives.ml

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(OPTCOMPFLAGS) -c $<

$(OBJS) std_exit.cmo: pervasives.cmi
$(OBJS:.cmo=.cmx) std_exit.cmx: pervasives.cmi

$(OBJS) std_exit.cmo: $(COMPILER)
$(OBJS:.cmo=.cmi) std_exit.cmi: $(COMPILER)
$(OBJS:.cmo=.cmx) std_exit.cmx: $(OPTCOMPILER)

clean::
	rm -f *.cm* *.o *.a
	rm -f *~

include .depend

depend:
	$(CAMLDEP) *.mli *.ml > .depend
