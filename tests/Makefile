
OCAMLC=../byterun/ocamlrun ../ocamlc -g -I ../stdlib
OCAMLOPT=../byterun/ocamlrun ../ocamlopt -I ../stdlib
OCAMLRUN=../byterun/ocamlrun

all: hello.byte backtrace.byte test_gc.byte test_foreign.byte

# special case, to illustrate
hello.byte: hello.ml
	$(OCAMLC) hello.ml -o hello.byte

hello.opt: hello.ml
	$(OCAMLOPT) hello.ml -o hello.opt


test_foreign.byte: foreign.o test_foreign.cmo 
	$(OCAMLC) -custom $^ -o $@


%.byte: %.ml
	$(OCAMLC) $^ -o $@

%.cmo: %.ml
	$(OCAMLC) -c $^

%.o: %.c
	gcc -I/usr/local/lib/ocaml -c $^


run:
	$(OCAMLRUN) ./hello.byte

clean:
	rm -f *.cm* *.o
	rm -f *.byte *.opt 


debug:
	../tools/misc/dumpobj backtrace.byte