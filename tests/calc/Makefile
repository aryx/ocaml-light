TOP=../..

OCAMLRUN=$(TOP)/byterun/ocamlrun
OCAMLC=$(OCAMLRUN) $(TOP)/ocamlc -g -I $(TOP)/stdlib
OCAMLLEX=$(OCAMLRUN) $(TOP)/lex/ocamllex

all: calc.byte

calc.byte: ast.cmo eval.cmo parser.cmo lexer.cmo main.cmo
	$(OCAMLC) -o $@ $^

lexer.ml: lexer.mll
	$(OCAMLLEX) $^
parser.ml parser.mli: parser.mly
	$(TOP)/yacc/ocamlyacc $^

# parser.mli

%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmo: %.ml
	$(OCAMLC) -c $<


clean:
	rm -f *.cm* lexer.ml parser.mli parser.ml *.byte

# manual dependencies
parser.cmo: parser.cmi
