# The lexer generator

COMPFLAGS=
LINKFLAGS=
CAMLC=../boot/ocamlrun ../boot/ocamlc -I ../boot
YACCFLAGS=
CAMLYACC=../boot/ocamlyacc
CAMLLEX=../boot/ocamlrun ../boot/ocamllex
DEPFLAGS=
CAMLDEP=../boot/ocamlrun ../tools/dependencies/ocamldep

OBJS=Ast.cmo Parser.cmo Lexer.cmo Lexgen.cmo Compact.cmo Output.cmo CLI.cmo Main.cmo

#pad: if want to compile with a more recent ocaml and use a recent ocamldebug
#CAMLLEX=ocamllex
#CAMLYACC=ocamlyacc
#CAMLC=ocamlc -g -I ../yacc2/compat
#OBJS=../yacc2/compat/set.cmo ../yacc2/compat/map.cmo parser.cmo lexer.cmo lexgen.cmo compact.cmo output.cmo main.cmo

all: ocamllex

ocamllex: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamllex $(OBJS)

clean::
	rm -f ocamllex
	rm -f *.cmo *.cmi

Parser.ml Parser.mli: Parser.mly
	$(CAMLYACC) $(YACCFLAGS) Parser.mly
	perl -p -i -e 's#/\*\(\*[sex]: .* \*\)\*/##' Parser.ml
#pad: this  perl thing is just because ocamlyacc does not handle well
# syncweb special comment at the very end

clean::
	rm -f Parser.ml Parser.mli

beforedepend:: Parser.ml Parser.mli

Lexer.ml: Lexer.mll
	$(CAMLLEX) Lexer.mll

clean::
	rm -f Lexer.ml

beforedepend:: Lexer.ml

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

depend: beforedepend
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
