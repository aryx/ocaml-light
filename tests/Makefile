
OCAMLC=../byterun/ocamlrun ../ocamlc -g -I ../stdlib
OCAMLOPT=../byterun/ocamlrun ../ocamlopt -I ../stdlib
OCAMLRUN=../byterun/ocamlrun

all: hello.byte backtrace.byte test_gc.byte

# special case, to illustrate
hello.byte: hello.ml
	$(OCAMLC) hello.ml -o hello.byte

hello.opt: hello.ml
	$(OCAMLOPT) hello.ml -o hello.opt


%.byte: %.ml
	$(OCAMLC) $^ -o $@


run:
	$(OCAMLRUN) ./hello.byte

clean:
	rm -f *.cm* *.o
	rm -f *.byte *.opt 


debug:
	../tools/misc/dumpobj backtrace.byte