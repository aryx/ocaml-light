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


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let output_parser def env lrtables chan =
  let pf x = Printf.fprintf chan x in
  let (action_table, goto_table) = lrtables in

  let harity = Hashtbl.create 13 in
  def.directives |> List.iter (function
    | Token (sopt, T t) ->
        Hashtbl.add harity t (match sopt with None -> 0 | Some _ -> 1)
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

  pf "\n";
  pf "open Parsing\n";

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

  pf "let lrtables = {\n";
  pf "  action = (function\n";
  action_table |> List.iter (fun ((S id, T t), action) ->
    (* if reached a state where there is dollar involved, means
     * we're ok!
     *)
    if t = "$"
    then pf "   | S %d, _ -> Accept\n" id
    else begin
      pf "   | S %d, %s%s -> " id t
        (match Hashtbl.find harity t with
        | 0 -> ""
        | _ -> " _"
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
  pf "  goto = (function\n";
  goto_table |> List.iter (fun ((S id1, NT nt), S id2) ->
    pf "  | S %d, NT \"%s\" -> S %d\n" id1 nt id2
  );


  pf "    | _ -> raise Parse_error\n";
  pf "  );\n";
  pf "}\n";


  let nt = Ast.start_symbol def in
  let (NT start) = nt in

  pf "let %s lexfun lexbuf =\n" start;
  pf "  Parsing.yyparse_simple lrtables lexfun string_of_token lexbuf\n";
  ()

