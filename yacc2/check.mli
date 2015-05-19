
type error = unit

exception Error of error

val check: Ast.parser_definition -> unit
