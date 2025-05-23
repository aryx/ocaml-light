(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


open Debugger_config
open Parameters
open Misc
open Primitives
open Source

(* Print a line; return the beginning of the next line *)
let print_line buffer line_number start point before =
  let next = next_linefeed buffer start
  and content = buffer_content buffer
  in
    print_int line_number;
    print_string " ";
    if (point <= next) & (point >= start) then
      (print_string (String.sub content start (point - start));
       print_string (if before then event_mark_before else event_mark_after);
       print_string (String.sub content point (next - point)))
    else
      print_string (String.sub content start (next - start));
    print_newline ();
    next

(* Tell Emacs we are nowhere in the source. *)
let show_no_point () =
  if !emacs then begin
    print_string "\026\026H";
    print_newline ()
    end

(* Print the line containing the point *)
let show_point mdle point before selected =
  if !emacs & selected then
    begin try
      let source = source_of_module mdle in
	print_string "\026\026M";
	print_string source;
	print_string ":";
	print_int point;
	print_string (if before then ":before" else ":after");
	print_newline ()
    with
      Not_found    -> (* get_buffer *)
        prerr_endline ("No source file for " ^ mdle ^ ".");
	show_no_point ()
    end
  else
    begin try
      let buffer = get_buffer mdle in
      let (start, line_number) = line_of_pos buffer point in
        print_line buffer line_number start point before; ()
    with
      Out_of_range -> (* line_of_pos *)
        prerr_endline "Position out of range."
    | Not_found    -> (* get_buffer *)
        prerr_endline ("No source file for " ^ mdle ^ ".")
    end

(* Display part of the source. *)
let show_listing mdle start stop point before =
  try
    let buffer = get_buffer mdle in
      let rec aff (line_start, line_number) =
        if line_number <= stop then
          aff (print_line buffer line_number line_start point before + 1, line_number + 1)
      in
        aff (pos_of_line buffer start)
  with
    Out_of_range -> (* pos_of_line *)
      prerr_endline "Position out of range."
  | Not_found    -> (* get_buffer *)
      prerr_endline ("No source file for " ^ mdle ^ ".")
