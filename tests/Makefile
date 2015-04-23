
OCAML=../boot/ocamlrun ../boot/ocamlc -I ../stdlib
OCAMLOPT=../boot/ocamlrun ../ocamlopt -I ../stdlib
OCAMLRUN=../boot/ocamlrun

hello: hello.ml
	$(OCAML) hello.ml

opt: hello.ml
	$(OCAMLOPT) hello.ml -o hello.opt


run:
	$(OCAMLRUN) ./a.out

clean:
	rm -f *.cm*
	rm -f a.out

