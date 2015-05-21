(*s: yacc2/check.mli *)

(*s: enum Check.error (yacc) *)
type error = unit
(*e: enum Check.error (yacc) *)

(*s: exception Check.Error (yacc) *)
exception Error of error
(*e: exception Check.Error (yacc) *)

(*s: signature Check.check (yacc) *)
val check: Ast.parser_definition -> unit
(*e: signature Check.check (yacc) *)
(*e: yacc2/check.mli *)
