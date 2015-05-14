let main () =
  let file = Sys.argv.(1) in
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  try 
    let ast = Parser.expr Lexer.token lexbuf in
    let i = Eval.eval ast in
    Printf.printf "result = %d\n" i
  with Parsing.Parse_error ->
    Printf.printf "parse error at character position %d on %s\n" 
      (Lexing.lexeme_start lexbuf) (Lexing.lexeme lexbuf)

let _ = main ()
