
type charpos = int
type location =
    Location of charpos * charpos

(* todo: at some point need to parse to extract the $xxx *)
type action = location

(* todo: in lrtables.ml at some point *)
type term = T of string
type nonterm = NT of string
type symbol = Term of term | Nonterm of nonterm

type grammar = rule_ list

  and rule_ = {
    lhs_: nonterm; (* lhs conflict with ocamlyacc yytable record *)
    rhs: symbol list;
    act: action;
  }

type directive =
  | Token of type_ option * term
  | Start of nonterm
  | Type of type_ * nonterm

  | Prec of unit (* TODO *)

  and type_ = string

type parser_definition = {
  header: location;
  directives: directive list;
  grm: grammar;
  trailer: location;
}
