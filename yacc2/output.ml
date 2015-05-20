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
open Lr0
open Lrtables

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let copy_buffer = String.create 1024

let get_chunk ic (Location(start,stop)) =
  seek_in ic start;
  let buf = Buffer.create 1024 in

  let n = ref (stop - start) in
  while !n > 0 do
    let m = input ic copy_buffer 0 (min !n 1024) in
    Buffer.add_string buf (String.sub copy_buffer 0 m);
    n := !n - m
  done;
  Buffer.contents buf

let copy_chunk ic oc (Location(start,stop)) =
  seek_in ic start;
  let n = ref (stop - start) in
  while !n > 0 do
    let m = input ic copy_buffer 0 (min !n 1024) in
    output oc copy_buffer 0 m;
    n := !n - m
  done


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let output_parser def env lrtables ic oc =
  let pf x = Printf.fprintf oc x in
  let (action_table, goto_table) = lrtables in

  let htype = Hashtbl.create 13 in
  def.directives |> List.iter (function
    | Token (sopt, T t) ->
        Hashtbl.add htype t sopt
    | _ -> ()
  );

  pf "type token =\n";
  def.directives |> List.iter (function
    | Token (sopt, (T s)) ->
      pf " | %s%s\n" s
        (match sopt with
        | None -> ""
        | Some s -> Printf.sprintf " of %s" s
        )
    | _ -> ()
  );

  copy_chunk ic oc def.header;
  pf "\n";
  pf "let user_actions = [|\n";
  env.g |> Array.iteri (fun i e ->
    if i = 0
    then pf "  (fun __parser_env -> failwith \"parser\");\n"
    else begin
      let s = get_chunk ic e.act in
      (* replace $ *)
      pf "  (fun __parser_env -> \n";
      pf "    Obj.repr((\n";
      pf "      "; 
      pf "%s" s;
      pf "   )";
    (* todo: if type info *)
      pf ")\n";
      pf "   );\n";
    end
  );
  pf "|]\n";
  copy_chunk ic oc def.trailer;


  pf "\n";
  pf "open Parsing\n";

  (* for debugging support *)
  pf "let string_of_token = function\n";
  def.directives |> List.iter (function
    | Token (sopt, (T s)) ->
      pf " | %s%s -> \"%s\"\n" s
        (match sopt with
        | None -> ""
        | Some s -> " _"
        )
        s
    | _ -> ()
  );
  pf "\n";

  (* the main tables *)
  pf "let lrtables = {\n";

  (* the action table *)
  pf "  action = (function\n";
  action_table |> List.iter (fun ((S id, T t), action) ->
    (* if reached a state where there is dollar involved, means
     * we're ok!
     *)
    if t = "$"
    then pf "   | S %d, _ -> Accept\n" id
    else begin
      pf "   | S %d, %s%s -> " id t
        (match Hashtbl.find htype t with
        | None -> ""
        | Some _ -> " _"
        );
      (match action with
      | Shift (S id) -> pf "Shift (S %d)" id
      | Accept -> pf "Accept"
      | Reduce (R ridx) ->
          let r = env.g.(ridx) in
          let n = List.length r.rhs in
          let (NT l) = r.lhs in
          pf "Reduce (%d, NT \"%s\", RA \"%s\")" n l l
      | Error -> failwith "Error should not be in action tables"
      );
      pf "\n";
    end
  );

  pf "    | _ -> raise Parse_error\n";
  pf "  );\n";


  (* the goto table *)
  pf "  goto = (function\n";
  goto_table |> List.iter (fun ((S id1, NT nt), S id2) ->
    pf "  | S %d, NT \"%s\" -> S %d\n" id1 nt id2
  );
  pf "    | _ -> raise Parse_error\n";
  pf "  );\n";
  pf "}\n";


  (* the main entry point *)
  let nt = Ast.start_symbol def in
  let (NT start) = nt in

  pf "let %s lexfun lexbuf =\n" start;
  pf "  Parsing.yyparse_simple lrtables lexfun string_of_token lexbuf\n";
  ()

