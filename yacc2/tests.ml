open Ast
open Lr0

(* from tests/yacc/arith.mly, a copy of the representative grammar in
 * the dragon book in 4.1
 * 
 * E -> E + T | T
 * T -> T * F | F
 * F -> ( E ) | id
 *)
let noloc = Location (0, 0)
let arith =
    [{lhs_ = NT "e";
      rhs = [Nonterm (NT "e"); Term (T "PLUS");  Nonterm (NT "t")];
      act = noloc};
     {lhs_ = NT "e"; 
      rhs = [Nonterm (NT "t")];
      act = noloc};
     {lhs_ = NT "t";
      rhs = [Nonterm (NT "t"); Term (T "MULT"); Nonterm (NT "f")];
      act = noloc};
     {lhs_ = NT "t"; rhs = [Nonterm (NT "f")];
      act = noloc};
     {lhs_ = NT "f";
      rhs = [Term (T "TPAR"); Nonterm (NT "e"); Term (T "TCPAR")];
      act = noloc};
     {lhs_ = NT "f"; 
      rhs = [Term (T "ID")];
      act = noloc}]

let augmented_arith =
  {lhs_ = NT "e'"; rhs = [Nonterm (NT "e")]; act = noloc} :: arith


module Set = Set_poly

let test_lr0 () =
  let env = { g = Array.of_list augmented_arith } in
  let items = Set.singleton (R 0, D 0) in
  let i0 = Lr0.closure env items in
  let xs = Set.elements i0 in
  ()
