
OCAML=../boot/ocamlrun ../boot/ocamlc -I ../stdlib
OCAMLOPT=../boot/ocamlrun ../ocamlopt -I ../stdlib
OCAMLRUN=../boot/ocamlrun

hello.byte: hello.ml
	$(OCAML) hello.ml -o hello.byte

hello.opt: hello.ml
	$(OCAMLOPT) hello.ml -o hello.opt


run:
	$(OCAMLRUN) ./hello.byte

clean:
	rm -f *.cm* *.o
	rm -f *.byte *.opt 


