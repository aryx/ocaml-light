; foo 
(setq 
 pad-ocaml-project-path "/home/pad/github/fork-ocaml"
 pad-ocaml-project-subdirs 
 (split-string 
  "lex yacc yacc2
  "
  )
 pad-ocaml-project-toplevel "pfff.top"
)


(setq
 pad-ocaml-project-prog     "lex/ocamllex"
 pad-ocaml-project-args 
 (join-string 
  (list 
   ""
   (case 0
     
     (0 "/home/pad/github/fork-ocaml/tests/calc/lexer.mll")
     
     )
   ))
 )

(setq
 pad-ocaml-project-prog     "yacc2/ocamlyacc"
 pad-ocaml-project-args 
 (join-string 
  (list 
   ""
   (case 1
     
     (0 "/home/pad/github/fork-ocaml/tests/calc/parser.mly")
     (1 "/home/pad/github/fork-ocaml/tests/yacc/arith.mly")
     
     )
   ))
 )
