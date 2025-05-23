(*s: parsing/location.ml *)
(*s: copyright header *)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header *)

open Lexing

(*s: type [[Location.t]] *)
(* Source code locations, used in parsetree *)

type t =
  { loc_start: int; loc_end: int }
(*e: type [[Location.t]] *)

(*s: constant [[Location.none]] *)
let none = { loc_start = -1; loc_end = -1 }
(*e: constant [[Location.none]] *)

(*s: function [[Location.symbol_loc]] *)
let symbol_loc () = 
  { loc_start = Parsing.symbol_start(); loc_end = Parsing.symbol_end() }
(*e: function [[Location.symbol_loc]] *)

(*s: function [[Location.rhs_loc]] *)
let rhs_loc n =
  { loc_start = Parsing.rhs_start n; loc_end = Parsing.rhs_end n }
(*e: function [[Location.rhs_loc]] *)

(*s: constant [[Location.input_name]] *)
let input_name = ref ""
(*e: constant [[Location.input_name]] *)

(*s: constant [[Location.input_lexbuf]] *)
let input_lexbuf = ref (None : lexbuf option)
(*e: constant [[Location.input_lexbuf]] *)

(*s: type [[Location.terminal_info_status]] *)
(* Terminal info *)

type terminal_info_status = Unknown | Bad_term | Good_term
(*e: type [[Location.terminal_info_status]] *)

let status = ref Unknown
and num_lines = ref 0
and cursor_up = ref ""
and cursor_down = ref ""
and start_standout = ref ""
and end_standout = ref ""

(*s: function [[Location.setup_terminal_info]] *)
let setup_terminal_info() =
  try
    Terminfo.setupterm();
    num_lines := Terminfo.getnum "li";
    cursor_up := Terminfo.getstr "up";
    cursor_down := Terminfo.getstr "do";
    begin try
      start_standout := Terminfo.getstr "us";
      end_standout := Terminfo.getstr "ue"
    with Not_found ->
      start_standout := Terminfo.getstr "so";
      end_standout := Terminfo.getstr "se"
    end;
    status := Good_term
  with _ ->
    status := Bad_term
(*e: function [[Location.setup_terminal_info]] *)

(*s: constant [[Location.num_loc_lines]] *)
(* Print the location using standout mode. *)

let num_loc_lines = ref 0 (* number of lines already printed after input *)
(*e: constant [[Location.num_loc_lines]] *)

(*s: function [[Location.highlight_locations]] *)
let rec highlight_locations loc1 loc2 =
  match !status with
    Unknown ->
      setup_terminal_info(); highlight_locations loc1 loc2
  | Bad_term ->
      false
  | Good_term ->
      match !input_lexbuf with
        None -> false
      | Some lb ->
          (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
          let pos0 = -lb.lex_abs_pos in
          (* Do nothing if the buffer does not contain the whole phrase. *)
          if pos0 < 0 then false else begin
            (* Count number of lines in phrase *)
            let lines = ref !num_loc_lines in
            for i = pos0 to Bytes.length lb.lex_buffer - 1 do
              if Bytes.get lb.lex_buffer i = '\n' then incr lines
            done;
            (* If too many lines, give up *)
            if !lines >= !num_lines - 2 then false else begin
              (* Move cursor up that number of lines *)
              for i = 1 to !lines do
                Terminfo.puts stdout !cursor_up 1
              done;
              (* Print the input, switching to standout for the location *)
              let bol = ref false in
          print_string "# ";
              for pos = 0 to Bytes.length lb.lex_buffer - pos0 - 1 do
                if !bol then (print_string "  "; bol := false);
                if pos = loc1.loc_start || pos = loc2.loc_start then
                  Terminfo.puts stdout !start_standout 1;
                if pos = loc1.loc_end || pos = loc2.loc_end then
                  Terminfo.puts stdout !end_standout 1;
                let c = Bytes.get lb.lex_buffer (pos + pos0) in
                print_char c;
                bol := (c = '\n')
              done;
              (* Make sure standout mode is over *)
              Terminfo.puts stdout !end_standout 1;
              (* Position cursor back to original location *)
              for i = 1 to !num_loc_lines do
                Terminfo.puts stdout !cursor_down 1
              done;
              true
            end
          end
(*e: function [[Location.highlight_locations]] *)

(* Print the location in some way or another *)

open Format

(*s: function [[Location.reset]] *)
let reset () =
  num_loc_lines := 0
(*e: function [[Location.reset]] *)

(*s: constants [[Location.msg_xxx]] *)
let (msg_file, msg_line, msg_chars, msg_to, msg_colon, warn_head) =
  match Sys.os_type with
  | _ -> ("File \"", "\", line ", ", characters ", "-", ":", "")
(*e: constants [[Location.msg_xxx]] *)

(*s: function [[Location.print]] *)
let print loc =
  if String.length !input_name = 0 then
    if highlight_locations loc none then () else begin
      print_string "Characters ";
      print_int loc.loc_start; print_string "-";
      print_int loc.loc_end; print_string ":";
      force_newline()
    end
  else begin
    let (filename, linenum, linebeg) =
            Linenum.for_position !input_name loc.loc_start in
    print_string msg_file; print_string filename;
    print_string msg_line; print_int linenum;
    print_string msg_chars; print_int (loc.loc_start - linebeg);
    print_string msg_to; print_int (loc.loc_end - linebeg);
    print_string msg_colon;
    force_newline()
  end
(*e: function [[Location.print]] *)

(*s: function [[Location.print_warning]] *)
let print_warning loc msg =
  let (f1, f2) = Format.get_formatter_output_functions() in
  if not !Sys.interactive then Format.set_formatter_out_channel stderr;
  print loc;
  print_string warn_head;
  print_string "Warning: "; print_string msg; print_newline();
  incr num_loc_lines;
  Format.set_formatter_output_functions f1 f2
(*e: function [[Location.print_warning]] *)

(*s: function [[Location.echo_eof]] *)
let echo_eof () =
  print_newline ();
  incr num_loc_lines
(*e: function [[Location.echo_eof]] *)

(*e: parsing/location.ml *)
