(*s: yacc2/lrtables.ml *)

(*s: enum Lrtables.action (yacc) *)
type action =
  | Shift of Lr0.stateid
  | Reduce of Lr0.ruleidx
  | Accept
  | Error
(*e: enum Lrtables.action (yacc) *)

(*s: enum Lrtables.action_table (yacc) *)
(* term can be also the special "$" terminal.
 * Everything not in the list is an Error action, so this
 * list should not contain any Error.
 *)
type action_table = 
    ((Lr0.stateid * Ast.term) * action) list
(*e: enum Lrtables.action_table (yacc) *)

(*s: enum Lrtables.goto_table (yacc) *)
type goto_table = 
    ((Lr0.stateid * Ast.nonterm) * Lr0.stateid) list
(*e: enum Lrtables.goto_table (yacc) *)
    
(*s: enum Lrtables.lr_tables (yacc) *)
type lr_tables = action_table * goto_table
(*e: enum Lrtables.lr_tables (yacc) *)

(*e: yacc2/lrtables.ml *)
