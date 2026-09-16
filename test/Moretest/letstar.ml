(* Tests for the let* / let+ binding-operator sugar (see
 * docs/claude_notes/plan_let_star.md). Desugaring happens at parse time,
 * so these mostly exercise the definition-site and use-site grammar
 * with a locally-defined operator; Stdlib itself doesn't ship
 * Option's or Result's let* / let+ (real OCaml only added those much
 * later, in 5.5), so each test defines its own one-liner from
 * bind/map, same as e.g. semgrep-pfff-libs/commons/core/Common.ml
 * does. *)

let check name cond =
  if cond then print_string ("OK " ^ name ^ " ") else print_string ("FAILED " ^ name ^ " ")

(* Option chaining *)
let () =
  let ( let* ) = Option.bind in
  let ( let+ ) o f = Option.map f o in
  let r =
    let* x = Some 1 in
    let* y = Some 2 in
    let+ z = Some 3 in
    x + y + z
  in
  check "option-chain" (r = Some 6)

let () =
  let ( let* ) = Option.bind in
  let r =
    let* x = Some 1 in
    let* _y = None in
    Some x
  in
  check "option-short-circuit" (r = None)

(* Result chaining (Result.map isn't exposed here, only bind) *)
let () =
  let ( let* ) = Result.bind in
  let r =
    let* x = Ok 1 in
    let* y = Ok 2 in
    Ok (string_of_int (x + y))
  in
  check "result-chain" (r = Ok "3")

let () =
  let ( let* ) = Result.bind in
  let r =
    let* x = Ok 1 in
    let* _y = Error "boom" in
    Ok x
  in
  check "result-short-circuit" (r = Error "boom")

(* Refutable pattern: matches -> fine *)
let () =
  let ( let* ) = Option.bind in
  let r =
    let* (a, b) = Some (1, 2) in
    Some (a + b)
  in
  check "refutable-pattern-match" (r = Some 3)

(* Refutable pattern that fails to match: documented gap vs real OCaml
 * (which would use the binding operator's failure protocol instead) --
 * this desugaring just raises Match_failure *)
let () =
  let ( let* ) = Option.bind in
  let raised =
    try
      let _ =
        let* (Some x) = Some None in
        Some x
      in
      false
    with Match_failure _ -> true
  in
  check "refutable-pattern-mismatch-raises" raised

(* Definition-site grammar: a user-defined let*/let+ operator *)
let () =
  let ( let* ) x f = f x in
  let ( let+ ) x f = f x in
  let r =
    let* x = 1 in
    let+ y = 2 in
    x + y
  in
  check "custom-operator-definition" (r = 3)

let () = print_newline ()
