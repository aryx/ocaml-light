##############################################################################
# Configuration
##############################################################################

TOP=..

OCAMLC=$(TOP)/byterun/ocamlrun $(TOP)/ocamlc -g -I $(TOP)/stdlib
OCAMLOPT=$(TOP)/byterun/ocamlrun $(TOP)/ocamlopt -I $(TOP)/stdlib
OCAMLRUN=$(TOP)/byterun/ocamlrun

##############################################################################
# Main rules
##############################################################################

default:
	$(MAKE) all
	$(MAKE) test

all: hello.byt backtrace.byt test_gc.byt test_foreign.byt
#TODO: .byte -> .opt

#TODO: see ../test/Makefile for inspiration
test:
	@echo TODO: we should compare actual output to expected output

##############################################################################
# Programs
##############################################################################

# special case, to illustrate
hello.byt: hello.ml
	$(OCAMLC) hello.ml -o hello.byt

hello.opt: hello.ml
	$(OCAMLOPT) hello.ml -o hello.opt


test_foreign.byt: foreign.o test_foreign.cmo 
	$(OCAMLC) -custom $^ -o $@


##############################################################################
# Common rules
##############################################################################

%.byt: %.ml
	$(OCAMLC) $^ -o $@

%.cmo: %.ml
	$(OCAMLC) -c $^

#alt: -I /usr/local/lib/ocaml
%.o: %.c
	gcc -g -I $(TOP)/byterun -c $^

clean:
	rm -f *.cm* *.o
	rm -f *.byt *.opt 

##############################################################################
# Developer rules
##############################################################################

run:
	$(OCAMLRUN) ./hello.byt

debug:
	../tools/misc/dumpobj backtrace.byt
