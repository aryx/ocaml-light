; foo 
(setq 
 pad-ocaml-project-path "/home/pad/github/fork-ocaml"
 pad-ocaml-project-subdirs 
 (split-string 
  "lex
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
