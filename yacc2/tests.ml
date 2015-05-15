open Ast
open Lr0

module Set = Set_poly

(* from tests/yacc/arith.mly which is a copy of the representative grammar in
 * the dragon book in 4.1
 * E' -> E         (R0)
 * E -> E + T | T  (R1, R2)
 * T -> T * F | F  (R3, R4)
 * F -> ( E ) | id (R5, R6)
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
      rhs = [Term (T "TOPAR"); Nonterm (NT "e"); Term (T "TCPAR")];
      act = noloc};
     {lhs_ = NT "f"; 
      rhs = [Term (T "ID")];
      act = noloc}]
let augmented_arith =
  {lhs_ = NT "e'"; rhs = [Nonterm (NT "e")]; act = noloc} :: arith



let test_lr0 () =
  let env = { g = Array.of_list augmented_arith } in

  (* closure *)
  let items = Set.singleton (R 0, D 0) in
  let i0 = Lr0.closure env items in
  let _xs = Set.elements i0 in
  (* [(R 0, D 0); (R 1, D 0); (R 2, D 0); (R 3, D 0); (R 4, D 0); (R 5, D 0);
   (R 6, D 0)] *)
  
  (* goto *)
  let items = Set.of_list [(R 0, D 1); (R 1, D 1)] in
  let i6 = Lr0.goto env items (Term (T "PLUS")) in
  let _xs = Set.elements i6 in
  (* [(R 1, D 2); (R 3, D 0); (R 4, D 0); (R 5, D 0); (R 6, D 0)] *)
  ()
