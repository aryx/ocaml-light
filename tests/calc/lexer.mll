{
open Parser
}

let integer = ['0'-'9']+

rule token = parse
  | "#" { comment lexbuf }
  | [' ''\t''\n']+ { token lexbuf }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT }
  | "/" { DIV }
  | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "(" { OPAR }
  | ")" { CPAR }

  | eof { EOF }
  | _ { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }

and comment = parse
    [^'\n']+ { comment lexbuf }
  | '\n'     { token lexbuf }

  | eof { EOF }
  | _ { failwith ("unknown token in comment: " ^ Lexing.lexeme lexbuf) }
