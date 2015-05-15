open Ast

(* the index of the rule in env.g *)
type ruleidx = R of int
(* the dot position in the rhs of a rule *)
type dotidx = D of int

(* as mentionned in the dragon book *)
type item = ruleidx * dotidx

module Set = Set_poly

(* a.k.a an LR0 state *)
type items = item Set.t

type env = {
  g: Ast.rule_ array;
}

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
  let rhs = r.rhs in
  try
    Some (List.nth r.rhs idx)
  with Failure _ -> None




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

