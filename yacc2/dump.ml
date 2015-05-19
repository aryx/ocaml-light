open Format

open Ast
open Lr0

module Set = Set_poly
module Map = Map_poly

(* TODO: does not indent things correctly, even after an
 * open_box 2; I don't understand
 *)

let string_of_symbol s =
  match s with
  (* in ocamlyacc terminals are constructors and so are uppercase and
   * so non terminals are lowercase, but it's the opposite convention
   * used in the dragon book, so here we dump in the dragon book
   * way so it's easier to compare what we generate with what
   * the dragon book says we should generate
   *)
  | Nonterm (NT s) -> String.uppercase s
  | Term (T s) ->
    (match s with
    (* special cases for arith.mly and tests.ml grammar toy examples *)
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

  dump_symbol (Nonterm r.lhs);
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
  if didx = List.length r.rhs then begin
      print_string ".";
  end;

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

let dump_lr0_automaton env auto =

  open_box 0;

  (* the states *)
  auto.int_to_state |> Array.iteri (fun i items ->
    print_string "I"; print_int i; print_newline ();
    open_box 2;
    dump_items env items;
    close_box ();
    print_newline ();
  );

  (* the transitions *)
  auto.trans |> Map.iter (fun (items1, symb) items2 ->
    let (S src) = Map.find items1 auto.state_to_int in
    let (S dst) = Map.find items2 auto.state_to_int in
    print_string "I"; print_int src;
    print_string " --"; dump_symbol symb; print_string "-->";
    print_string " I"; print_int dst;
    print_newline ()
  );

  close_box ()


let dump_lrtables env lrtables =
  failwith "TODO"
