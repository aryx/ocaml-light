(*s: asmcomp/asmgen.ml *)
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

(* From lambda to assembly code *)

open Format
open Config
open Clflags
open Misc
open Cmm

(*s: type [[Asmgen.error]]([[(asmcomp/asmgen.ml)]]) *)
type error = Assembler_error of string
(*e: type [[Asmgen.error]]([[(asmcomp/asmgen.ml)]]) *)

(*s: exception [[Asmgen.Error]]([[(asmcomp/asmgen.ml)]]) *)
exception Error of error
(*e: exception [[Asmgen.Error]]([[(asmcomp/asmgen.ml)]]) *)

(*s: function [[Asmgen.liveness]] *)
let liveness phrase =
  Liveness.fundecl phrase; phrase
(*e: function [[Asmgen.liveness]] *)

(*s: function [[Asmgen.dump_if]] *)
let dump_if flag message phrase =
  if !flag then Printmach.phase message phrase;
  phrase
(*e: function [[Asmgen.dump_if]] *)

(*s: function [[Asmgen.dump_linear_if]] *)
let dump_linear_if flag message phrase =
  if !flag then begin
    print_string "*** "; print_string message; print_newline();
    Printlinear.fundecl phrase; print_newline()
  end;
  phrase
(*e: function [[Asmgen.dump_linear_if]] *)

(*s: function [[Asmgen.regalloc]] *)
let rec regalloc round fd =
  if round > 50 then
    fatal_error(fd.Mach.fun_name ^
                ": function too complex, cannot complete register allocation");
  dump_if dump_live "Liveness analysis" fd |> ignore;
  Interf.build_graph fd;
  if !dump_interf then Printmach.interferences();
  if !dump_prefer then Printmach.preferences();
  Coloring.allocate_registers();
  dump_if dump_regalloc "After register allocation" fd |> ignore;
  let (newfd, redo_regalloc) = Reload.fundecl fd in
  dump_if dump_reload "After insertion of reloading code" newfd |> ignore;
  if redo_regalloc 
  then begin Reg.reinit(); Liveness.fundecl newfd; regalloc (round+1) newfd end
  else newfd
(*e: function [[Asmgen.regalloc]] *)

(*s: function [[Asmgen.TODOOPERATOR]] *)
let (++) x f = f x
(*e: function [[Asmgen.TODOOPERATOR]] *)

(*s: function [[Asmgen.compile_fundecl]] *)
let compile_fundecl fd_cmm =
  Reg.reset();
  fd_cmm
  ++ Selection.fundecl
  ++ dump_if dump_selection "After instruction selection"
  ++ liveness
  ++ dump_if dump_live "Liveness analysis"
  ++ Spill.fundecl
  ++ liveness
  ++ dump_if dump_spill "After spilling"
  ++ Split.fundecl
  ++ dump_if dump_split "After live range splitting"
  ++ liveness
  ++ regalloc 1
  ++ Linearize.fundecl
  ++ dump_linear_if dump_linear "Linearized code"
  ++ Scheduling.fundecl
  ++ dump_linear_if dump_scheduling "After instruction scheduling"
  ++ Emit.fundecl
(*e: function [[Asmgen.compile_fundecl]] *)

(*s: function [[Asmgen.compile_phrase]] *)
let compile_phrase p =
  if !dump_cmm then begin Printcmm.phrase p; print_newline() end;
  match p with
    Cfunction fd -> compile_fundecl fd
  | Cdata dl -> Emit.data dl
(*e: function [[Asmgen.compile_phrase]] *)

(*s: function [[Asmgen.compile_implementation]] *)
let compile_implementation prefixname size lam =
  let asmfile =
    if !keep_asm_file
    then prefixname ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm in
  let oc = open_out asmfile in
  begin try
    Emitaux.output_channel := oc;
    Emit.begin_assembly();
    List.iter compile_phrase (Cmmgen.compunit size (Closure.intro size lam));
    Emit.end_assembly();
    close_out oc
  with x ->
    close_out oc;
    if !keep_asm_file then () else remove_file asmfile;
    raise x
  end;
  if Proc.assemble_file asmfile (prefixname ^ ext_obj) <> 0
  then raise(Error(Assembler_error asmfile));
  if !keep_asm_file then () else remove_file asmfile
(*e: function [[Asmgen.compile_implementation]] *)

(*s: function [[Asmgen.report_error]] *)
(* Error report *)

let report_error = function
    Assembler_error file ->
      print_string "Assembler error, input left in file ";
      print_string file
(*e: function [[Asmgen.report_error]] *)
(*e: asmcomp/asmgen.ml *)
