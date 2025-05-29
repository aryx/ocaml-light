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

(* $Id$ *)

open Clflags

(* alt: was in codegen.ml and codegen.mli before:
(* From C-- to assembly code *)

val phrase: Cmm.phrase -> unit
val file: string -> unit

open Format
open Cmm

let rec regalloc fd =
  if !dump_live then Printmach.phase "Liveness analysis" fd;
  Interf.build_graph fd;
  if !dump_interf then Printmach.interferences();
  if !dump_prefer then Printmach.preferences();
  Coloring.allocate_registers();
  if !dump_regalloc then
    Printmach.phase "After register allocation" fd;
  let (newfd, redo_regalloc) = Reload.fundecl fd in
  if !dump_reload then
    Printmach.phase "After insertion of reloading code" newfd;
  if redo_regalloc 
  then begin Reg.reinit(); Liveness.fundecl newfd; regalloc newfd end
  else newfd

let fundecl fd_cmm =
  if !dump_cmm then begin
    print_string "*** C-- code"; print_newline();
    Printcmm.fundecl fd_cmm; print_newline()
  end;
  Reg.reset();
  let fd_sel = Sequence.fundecl fd_cmm in
  if !dump_selection then
    Printmach.phase "After instruction selection" fd_sel;
  Liveness.fundecl fd_sel;
  if !dump_live then Printmach.phase "Liveness analysis" fd_sel;
  let fd_spill = Spill.fundecl fd_sel in
  Liveness.fundecl fd_spill;
  if !dump_spill then
    Printmach.phase "After spilling" fd_spill;
  let fd_split = Split.fundecl fd_spill in
  Liveness.fundecl fd_split;
  if !dump_split then
    Printmach.phase "After live range splitting" fd_split;
  let fd_reload = regalloc fd_split in
  let fd_linear = Linearize.fundecl fd_reload in
  if !dump_linear then begin
    print_string "*** Linearized code"; print_newline();
    Printlinear.fundecl fd_linear; print_newline()
  end;
  Emit.fundecl fd_linear

let phrase = function
    Cfunction fd -> fundecl fd
  | Cdata dl -> Emit.data dl

let file filename =
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  try
    while true do
      phrase(Parsecmm.phrase Lexcmm.token lb)
    done
  with
      End_of_file ->
        close_in ic
    | Lexcmm.Error msg ->
        close_in ic; Lexcmm.report_error lb msg
    | Parsing.Parse_error ->
        close_in ic;
        prerr_string "Syntax error near character ";
        prerr_int (Lexing.lexeme_start lb);
        prerr_newline()
    | Parsecmmaux.Error msg ->
        close_in ic; Parsecmmaux.report_error msg
    | x ->
        close_in ic; raise x
*)


let compile_file filename =
  Compilenv.reset "test";
  Emit.begin_assembly();
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  try
    while true do
      Asmgen.compile_phrase(Parsecmm.phrase Lexcmm.token lb)
    done
  with
      End_of_file ->
        close_in ic; Emit.end_assembly()
    | Lexcmm.Error msg ->
        close_in ic; Lexcmm.report_error lb msg
    | Parsing.Parse_error ->
        close_in ic;
        prerr_string "Syntax error near character ";
        prerr_int (Lexing.lexeme_start lb);
        prerr_newline()
    | Parsecmmaux.Error msg ->
        close_in ic; Parsecmmaux.report_error msg
    | x ->
        close_in ic; raise x

let usage = "Usage: codegen <options> <files>\noptions are:"

let main() =
  Arg.parse [
     "-dcmm", Arg.Set dump_cmm, "";
     "-dsel", Arg.Set dump_selection, "";
     "-dlive", Arg.Unit(fun () -> dump_live := true;
                                  Printmach.print_live := true), "";
     "-dspill", Arg.Set dump_spill, "";
     "-dsplit", Arg.Set dump_split, "";
     "-dinterf", Arg.Set dump_interf, "";
     "-dprefer", Arg.Set dump_prefer, "";
     "-dalloc", Arg.Set dump_regalloc, "";
     "-dreload", Arg.Set dump_reload, "";
     "-dscheduling", Arg.Set dump_scheduling, "";
     "-dlinear", Arg.Set dump_linear, ""
    ] compile_file usage

let _ = Printexc.catch main (); exit 0

