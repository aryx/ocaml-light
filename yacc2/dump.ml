open Format

open Ast
open Lr0

module Set = Set_poly
module Map = Map_poly

let string_of_symbol s =
  match s with
  | Nonterm (NT s) -> String.uppercase s
  | Term (T s) ->
    (match s with
    (* special case for arith.mly and tests.ml grammar toy examples *)
    | "PLUS" -> "+"
    | "MULT" -> "*"
    | "TOPAR" -> "("
    | "TCPAR" -> ")"
    | _ -> String.lowercase s
    )
let dump_symbol s =
  print_string (string_of_symbol s)

let dump_item env item =
  let (R idx, D didx) = item in
  let r = env.g.(idx) in

  dump_symbol (Nonterm r.lhs_);
  print_space ();
  print_string "->";
  open_box 0;
  print_space ();
  r.rhs |> Array.of_list |> Array.iteri (fun i s ->
    if i = didx
    then begin 
      print_string ".";
      print_space ();
    end;
    dump_symbol s;
    print_space ();
  );
  close_box ();
(*  print_space (); print_string "(R"; print_int idx; print_string ")" *)
  ()

let dump_items env items =
  items 
  |> Set.elements |> List.sort (fun (R a, _) (R b, _) -> a - b)
  |> List.iter (fun item -> 
    open_box 0;
    dump_item env item;
    close_box ();
    print_newline ();
  )

let dump_lr0_automaton env (xxs, transitions) =

  let int_to_items = xxs |> Set.elements |> Array.of_list in
  let items_to_int = 
    let x = ref Map.empty in
    int_to_items |> Array.iteri (fun i items ->
      x := Map.add items i !x
    );
    !x
  in

  open_box 0;

  (* the states *)
  int_to_items |> Array.iteri (fun i items ->
    print_string "I"; print_int i; print_newline ();
    open_box 2;
    dump_items env items;
    close_box ();
    print_newline ();
  );

  (* the transitions *)
  transitions |> Map.iter (fun (items1, symb) items2 ->
    let src = Map.find items1 items_to_int in
    let dst = Map.find items2 items_to_int in
    print_string "I"; print_int src;
    print_string " --"; dump_symbol symb; print_string "-->";
    print_string " I"; print_int dst;
    print_newline ()
  );

  close_box ()
