
let main () =

  if Array.length Sys.argv != 2 then begin
    prerr_endline "Usage: ocamlyacc <input file>";
    exit 2
  end;

  let source_name = Sys.argv.(1) in

  let dest_name =
    if Filename.check_suffix source_name ".mly" 
    then Filename.chop_suffix source_name ".mly" ^ ".ml"
    else source_name ^ ".ml" 
  in
  let ic = open_in source_name in
  let oc = open_out dest_name in
  let lexbuf = Lexing.from_channel ic in

  (* parsing *)
  let def =
    try
      Parser.parser_definition Lexer.main lexbuf
    with exn ->
      close_out oc;
      Sys.remove dest_name;
       (match exn with
         Parsing.Parse_error ->
           prerr_string "Syntax error around char ";
           prerr_int (Lexing.lexeme_start lexbuf);
           prerr_endline "."
       | Lexer.Lexical_error s ->
           prerr_string "Lexical error around char ";
           prerr_int (Lexing.lexeme_start lexbuf);
           prerr_string ": ";
           prerr_string s;
           prerr_endline "."
       | _ -> raise exn
       );
      exit 2 
  in
  ()

let _ = 
  Tests.test_lr0 ();
  Printexc.catch main (); 
  exit 0
