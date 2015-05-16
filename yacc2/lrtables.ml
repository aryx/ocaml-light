
type action =
  | Shift of Lr0.stateid
  | Reduce of Lr0.ruleidx
  | Accept
  | Error

(* term can be also the special "$" terminal.
 * Everything not in the list is an Error action, so this
 * list should not contain any Error.
 *)
type action_table = 
    ((Lr0.stateid * Ast.term) * action) list

type goto_table = 
    ((Lr0.stateid * Ast.nonterm) * Lr0.stateid) list
    
type lr_tables = action_table * goto_table

let dollar_terminal = Ast.T "$"
