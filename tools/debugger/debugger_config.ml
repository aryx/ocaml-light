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


(**************************** Configuration file ***********************)

exception Toplevel

(*** Miscellaneous parameters. ***)

(*ISO 6429 color sequences
00      to restore default color
01      for brighter colors 
04      for underlined text
05      for flashing text
30      for black foreground
31      for red foreground
32      for green foreground
33      for yellow (or brown) foreground
34      for blue foreground
35      for purple foreground
36      for cyan foreground
37      for white (or gray) foreground
40      for black background
41      for red background
42      for green background
43      for yellow (or brown) background
44      for blue background
45      for purple background
46      for cyan background
47      for white (or gray) background
let debugger_prompt   = "\027[1;04m(ocd)\027[0m "
and event_mark_before = "\027[1;31m$\027[0m"
and event_mark_after  = "\027[1;34m$\027[0m"
*)
let debugger_prompt   = "(ocd) "
let event_mark_before = "<|b|>"
let event_mark_after  = "<|a|>"

(* Name of shell used to launch the debuggee *)
let shell = "/bin/sh"

(* Name of the Objective Caml runtime. *)
(* let runtime_program = "ocamlrun" *)
let runtime_program = "/home/mouton/xleroy/csl-debugger/byterun/ocamlrun"

(* Time history size (for `last') *)
let history_size = ref 30

(*** Time travel parameters. ***)

(* Step between checkpoints for long displacements.*)
let checkpoint_big_step = ref 10000

(* Idem for small ones. *)
let checkpoint_small_step = ref 1000

(* Maximum number of checkpoints. *)
let checkpoint_max_count = ref 15

(* Whether to keep checkpoints or not. *)
let make_checkpoints = ref true

(*** Dynamic loader ***)

(* List of standard library modules *)
let stdlib_units = [
  "Pervasives"; "List"; "Char"; "String"; "Array"; "Sys";
  "Hashtbl"; "Filename"; "Obj"; "Lexing"; "Parsing";
  "Set"; "Map"; "Stack"; "Queue"; "Stream";
  "Printf"; "Format"; "Arg"; "Printexc"; "Gc";
  "Digest"; "Random"; "Oo"; "Callback"
]
