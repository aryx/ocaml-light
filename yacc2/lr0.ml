(* Yoann Padioleau
 *
 * Copyright (C) 2015 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Computing the LR(0) automaton for a context free grammar, using
 * the algorithm described in the dragon book in chapter 4.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the index of the rule in env.g *)
type ruleidx = R of int
(* the dot position in the rhs of a rule *)
type dotidx = D of int

type stateid = S of int

(* as mentionned in the dragon book *)
type item = ruleidx * dotidx

module Set = Set_poly
module Map = Map_poly

(* a.k.a an LR0 state *)
type items = item Set.t

type env = {
  (* augmented grammar where r0 is e' -> start_original_grammar *)
  g: Ast.rule_ array;
}

type automaton = {
  states: items Set_poly.t;
  (* state 0 is the starting state *)
  int_to_state: items array;
  state_to_int: (items, stateid) Map_poly.t;
  (* goto mapping *)
  trans: (items * Ast.symbol, items) Map.t;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rule_at (R idx) env =
  env.g.(idx)

let rules_of nt env =
  let res = ref [] in
  env.g |> Array.iteri (fun idx r ->
    if r.lhs_ = nt
    then res := (R idx) :: !res
  );
  List.rev !res

let after_dot r (D idx) =
  try Some (List.nth r.rhs idx)
  with Failure _ -> None

let move_dot_right (D idx) = 
  (D (idx + 1))

let all_symbols env =
  env.g |> Array.fold_left (fun acc r ->
    ((Nonterm r.lhs_)::r.rhs) |> List.fold_left (fun acc symbol ->
      Set.add symbol acc
      ) acc
  ) Set.empty


(*****************************************************************************)
(* Algorithms *)
(*****************************************************************************)

(* opti: use kernel items *)
let closure env items =
  let result = ref items in

  let added = ref true in
  while !added do
    added := false;

    !result |> Set.iter (fun item ->
      let ridx, didx = item in
      let r = rule_at ridx env in

      match after_dot r didx with
      | Some (Nonterm b) ->
          let rules_idx = rules_of b env in
          rules_idx |> List.iter (fun ridx ->
            let item = (ridx, D 0) in
            if not (Set.mem item !result) then begin
              added := true;
              result := Set.add item !result;
            end
          )
      | _ -> ()
    )
  done;
  !result


let goto env items symbol =
  let start =
    Set.fold (fun item acc ->
      let ridx, didx = item in
      let r = rule_at ridx env in
      match after_dot r didx with
      | Some symbol2 when symbol = symbol2 ->
          Set.add (ridx, move_dot_right didx) acc
      | _ -> acc
    ) items Set.empty
  in
  closure env start

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let canonical_lr0_automaton env =
  let start_item = (R 0, D 0) in
  let start_items = closure env (Set.singleton start_item) in
  let result = ref (Set.singleton start_items) in
  let transitions = ref Map.empty in
  let symbols = all_symbols env in
  
  let added = ref true in
  while !added do
    added := false;

    !result |> Set.iter (fun items ->
      symbols |> Set.iter (fun symb ->
        let itemset = goto env items symb in
        if not (Set.is_empty itemset) && not (Map.mem (items, symb)!transitions)
        then transitions := Map.add (items, symb) itemset !transitions;

        if not (Set.is_empty itemset) && not (Set.mem itemset !result) 
        then begin
          added := true;
          result := Set.add itemset !result;
        end
      )
    )
  done;
  let states = !result in
  let trans = !transitions in

  (* put start state first in the list of states *)
  let states_no_start = Set.remove start_items states |> Set.elements  in
  let states_list = start_items::states_no_start in

  let int_to_items = states_list |> Array.of_list in
  let items_to_int = 
    let x = ref Map.empty in
    int_to_items |> Array.iteri (fun i items ->
      x := Map.add items (S i) !x
    );
    !x
  in
  { states = states;
    int_to_state = int_to_items;
    state_to_int = items_to_int;
    trans = trans
  }
