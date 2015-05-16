# The parser generator

CAMLC=../boot/ocamlrun ../boot/ocamlc -I ../boot
COMPFLAGS=
LINKFLAGS=
CAMLYACC=../boot/ocamlyacc
YACCFLAGS=
CAMLLEX=../boot/ocamlrun ../boot/ocamllex
CAMLDEP=../boot/ocamlrun ../tools/misc/ocamldep
DEPFLAGS=

OBJS1=ast.cmo parser.cmo lexer.cmo check.cmo \
  lr0.cmo lrtables.cmo slr.cmo lalr.cmo \
  output.cmo dump.cmo \
  tests.cmo main.cmo

#pad: if want to compile with a more recent ocaml and use a recent ocamldebug
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
CAMLC=ocamlc -annot -g -I /home/pad/pfff/commons/ocamlextra
OBJS=/home/pad/pfff/commons/ocamlextra/set_poly.cmo /home/pad/pfff/commons/ocamlextra/map_poly.cmo $(OBJS1)

all: ocamlyacc

ocamlyacc: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o $@ $(OBJS)

clean::
	rm -f ocamlyacc
	rm -f *.cmo *.cmi

parser.ml parser.mli: parser.mly
	$(CAMLYACC) $(YACCFLAGS) parser.mly
	perl -p -i -e 's#/\*\(\*[sex]: .* \*\)\*/##' parser.ml
#pad: this  perl thing is just because ocamlyacc does not handle well
# syncweb special comment at the very end

clean::
	rm -f parser.ml parser.mli

beforedepend:: parser.ml parser.mli

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

clean::
	rm -f lexer.ml

beforedepend:: lexer.ml

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

depend: beforedepend
	$(CAMLDEP) *.mli *.ml > .depend

-include .depend
