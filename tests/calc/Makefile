
TOP=../..

OCAMLRUN=$(TOP)/byterun/ocamlrun
OCAMLC=$(OCAMLRUN) $(TOP)/ocamlc -g -I $(TOP)/stdlib

all: calc.byte

calc.byte: ast.cmo eval.cmo parser.cmo lexer.cmo main.cmo
	$(OCAMLC) -o $@ $^


lexer.ml: lexer.mll
	$(OCAMLRUN) $(TOP)/lex/ocamllex $^

parser.ml parser.mli: parser.mly
	$(TOP)/yacc/ocamlyacc $^

parser.cmo: parser.cmi


%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmo: %.ml
	$(OCAMLC) -c $<


clean:
	rm -f *.cm* lexer.ml parser.mli parser.ml *.byte


