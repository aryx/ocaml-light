open Ast
open Lr0

(* from tests/yacc/arith.mly which is a copy of the representative grammar in
 * the dragon book in 4.1
 * $S -> E         (R0)
 * E -> E + T | T  (R1, R2)
 * T -> T * F | F  (R3, R4)
 * F -> ( E ) | id (R5, R6)
 *)
let noloc = Location (0, 0)
let arith =
    [{lhs = NT "e";
      rhs = [Nonterm (NT "e"); Term (T "PLUS");  Nonterm (NT "t")];
      act = noloc};
     {lhs = NT "e"; 
      rhs = [Nonterm (NT "t")];
      act = noloc};
     {lhs = NT "t";
      rhs = [Nonterm (NT "t"); Term (T "MULT"); Nonterm (NT "f")];
      act = noloc};
     {lhs = NT "t"; rhs = [Nonterm (NT "f")];
      act = noloc};
     {lhs = NT "f";
      rhs = [Term (T "TOPAR"); Nonterm (NT "e"); Term (T "TCPAR")];
      act = noloc};
     {lhs = NT "f"; 
      rhs = [Term (T "ID")];
      act = noloc}]
(*
let augmented_arith =
  {lhs = NT "$S"; rhs = [Nonterm (NT "e")]; act = noloc} :: arith
*)


let test_lr0 () =
  let env = Lr0.mk_env_augmented_grammar (NT "e") arith in

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
  
let test_slr () =
  let env = Lr0.mk_env_augmented_grammar (NT "e") arith in

  (* automaton *)
  let auto = Lr0.canonical_lr0_automaton env in
  
  (* first, follow *)
  let (first, eps) = First_follow.compute_first arith in
  let follow = First_follow.compute_follow env (first, eps) in
  
  (* slr tables *)
  let tables = Slr.lr_tables env auto follow in

  Dump.dump_lrtables env tables;
  ()


(*
 * E -> E E'
 * E' -> + T E' | epsilon
 * T -> F T'
 * T' -> * F T' | epsilon
 * F -> ( E ) | id
 *)
let arith_ll = 
    [
     {lhs = NT "e";
      rhs = [Nonterm (NT "t"); Nonterm (NT "e'")];
      act = noloc};
     {lhs = NT "e'"; 
      rhs = [Term (T "PLUS"); Nonterm (NT "t"); Nonterm (NT "e'")];
      act = noloc};
     {lhs = NT "e'"; 
      rhs = [];
      act = noloc};
     {lhs = NT "t";
      rhs = [Nonterm (NT "f"); Nonterm (NT "t'")];
      act = noloc};
     {lhs = NT "t'"; 
      rhs = [Term (T "MULT"); Nonterm (NT "f"); Nonterm (NT "t'")];
      act = noloc};
     {lhs = NT "t'"; 
      rhs = [];
      act = noloc};
     {lhs = NT "f";
      rhs = [Term (T "TOPAR"); Nonterm (NT "e"); Term (T "TCPAR")];
      act = noloc};
     {lhs = NT "f"; 
      rhs = [Term (T "ID")];
      act = noloc}]

let test_first_follow () =
  let (first, eps) = First_follow.compute_first arith_ll in
  let first' = first |> Map.to_list |> List.map (fun (t, set) -> 
    t, Set.elements set)
  in
  let eps' = Set.elements eps in

  let env = Lr0.mk_env_augmented_grammar (NT "e") arith_ll in
  let follow = First_follow.compute_follow env (first, eps) in
  let follow' = follow |> Map.to_list |> List.map (fun (t, set) ->
    t, Set.elements set)
  in
  ()


(* what we should generate *)
let test_lr_engine () =
  failwith "TODO"
