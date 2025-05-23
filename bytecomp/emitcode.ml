(*s: bytecomp/emitcode.ml *)
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

(* Generation of bytecode + relocation information *)

open Config
open Misc
open Asttypes
open Lambda
open Instruct
open Opcodes


(*s: type [[Emitcode.reloc_info]] *)
(* Relocation information *)

type reloc_info =
    Reloc_literal of structured_constant    (* structured constant *)
  | Reloc_getglobal of Ident.t              (* reference to a global *)
  | Reloc_setglobal of Ident.t              (* definition of a global *)
  | Reloc_primitive of string               (* C primitive number *)
(*e: type [[Emitcode.reloc_info]] *)

(*s: type [[Emitcode.compilation_unit]] *)
(* Descriptor for compilation units *)

type compilation_unit =
  { cu_name: string;                    (* Name of compilation unit *)
    mutable cu_pos: int;                (* Absolute position in file *)
    cu_codesize: int;                   (* Size of code block *)
    cu_reloc: (reloc_info * int) list;  (* Relocation information *)
    cu_imports: (string * Digest.t) list; (* Names and CRC of intfs imported *)
    cu_primitives: string list;         (* Primitives declared inside *)

    mutable cu_force_link: bool;        (* Must be linked even if unref'ed *)

    mutable cu_debug: int;              (* Position of debugging info, or 0 *)
    cu_debugsize: int;                  (* Length of debugging info *)
 }
(*e: type [[Emitcode.compilation_unit]] *)

(* Format of a .cmo file:
     magic number (Config.cmo_magic_number)
     absolute offset of compilation unit descriptor
     block of relocatable bytecode
     compilation unit descriptor *)

(* Buffering of bytecode *)

let out_buffer = ref(Bytes.create 1024)
and out_position = ref 0

(*s: function [[Emitcode.out_word]] *)
let out_word b1 b2 b3 b4 =
  let p = !out_position in
  if p >= Bytes.length !out_buffer then begin
    let len = Bytes.length !out_buffer in
    let new_buffer = Bytes.create (2 * len) in
    String.blit (Bytes.to_string !out_buffer) 0 new_buffer 0 len;
    out_buffer := new_buffer
  end;
  String.unsafe_set !out_buffer p (Char.unsafe_chr b1);
  String.unsafe_set !out_buffer (p+1) (Char.unsafe_chr b2);
  String.unsafe_set !out_buffer (p+2) (Char.unsafe_chr b3);
  String.unsafe_set !out_buffer (p+3) (Char.unsafe_chr b4);
  out_position := p + 4
(*e: function [[Emitcode.out_word]] *)

(*s: function [[Emitcode.out]] *)
let out opcode =
  out_word opcode 0 0 0
(*e: function [[Emitcode.out]] *)

(*s: function [[Emitcode.out_int]] *)
let out_int n =
  out_word n (n asr 8) (n asr 16) (n asr 24)
(*e: function [[Emitcode.out_int]] *)

(*s: type [[Emitcode.label_definition]] *)
(* Handling of local labels and backpatching *)

type label_definition =
    Label_defined of int
  | Label_undefined of (int * int) list
(*e: type [[Emitcode.label_definition]] *)

(*s: constant [[Emitcode.label_table]] *)
let label_table  = ref ([| |] : label_definition array)
(*e: constant [[Emitcode.label_table]] *)

(*s: function [[Emitcode.extend_label_table]] *)
let extend_label_table needed =
  let new_size = ref(Array.length !label_table) in
  while needed >= !new_size do new_size := 2 * !new_size done;
  let new_table = Array.create !new_size (Label_undefined []) in
  Array.blit !label_table 0 new_table 0 (Array.length !label_table);
  label_table := new_table
(*e: function [[Emitcode.extend_label_table]] *)

(*s: function [[Emitcode.backpatch]] *)
let backpatch (pos, orig) =
  let displ = (!out_position - orig) asr 2 in
  !out_buffer.[pos] <- Char.unsafe_chr displ;
  !out_buffer.[pos+1] <- Char.unsafe_chr (displ asr 8);
  !out_buffer.[pos+2] <- Char.unsafe_chr (displ asr 16);
  !out_buffer.[pos+3] <- Char.unsafe_chr (displ asr 24)
(*e: function [[Emitcode.backpatch]] *)

(*s: function [[Emitcode.define_label]] *)
let define_label lbl =
  if lbl >= Array.length !label_table then extend_label_table lbl;
  match (!label_table).(lbl) with
    Label_defined _ ->
      fatal_error "Emitcode.define_label"
  | Label_undefined patchlist ->
      List.iter backpatch patchlist;
      (!label_table).(lbl) <- Label_defined !out_position
(*e: function [[Emitcode.define_label]] *)

(*s: function [[Emitcode.out_label_with_orig]] *)
let out_label_with_orig orig lbl =
  if lbl >= Array.length !label_table then extend_label_table lbl;
  match (!label_table).(lbl) with
    Label_defined def ->
      out_int((def - orig) asr 2)
  | Label_undefined patchlist ->
      (!label_table).(lbl) <-
         Label_undefined((!out_position, orig) :: patchlist);
      out_int 0
(*e: function [[Emitcode.out_label_with_orig]] *)

(*s: function [[Emitcode.out_label]] *)
let out_label l = out_label_with_orig !out_position l
(*e: function [[Emitcode.out_label]] *)

(*s: constant [[Emitcode.reloc_info]] *)
(* Relocation information *)

let reloc_info = ref ([] : (reloc_info * int) list)
(*e: constant [[Emitcode.reloc_info]] *)

(*s: function [[Emitcode.enter]] *)
let enter info =
  reloc_info := (info, !out_position) :: !reloc_info
(*e: function [[Emitcode.enter]] *)

let slot_for_literal sc =
  enter (Reloc_literal sc);
  out_int 0
and slot_for_getglobal id =
  enter (Reloc_getglobal id);
  out_int 0
and slot_for_setglobal id =
  enter (Reloc_setglobal id);
  out_int 0
and slot_for_c_prim name =
  enter (Reloc_primitive name);
  out_int 0

(*s: constant [[Emitcode.events]] *)
(* Debugging events *)

let events = ref ([] : Instruct.debug_event list)
(*e: constant [[Emitcode.events]] *)

(*s: function [[Emitcode.record_event]] *)
let record_event ev =
  ev.ev_pos <- !out_position;
  events := ev :: !events
(*e: function [[Emitcode.record_event]] *)

(*s: function [[Emitcode.init]] *)
(* Initialization *)

let init () =
  out_position := 0;
  label_table := Array.create 16 (Label_undefined []);
  reloc_info := [];
  events := []
(*e: function [[Emitcode.init]] *)

(*s: constant [[Emitcode.emit_instr]] *)
(* Emission of one instruction *)

let emit_instr = function
    Klabel lbl -> define_label lbl
  | Kacc n ->
      if n < 8 then out(opACC0 + n) else (out opACC; out_int n)
  | Kenvacc n ->
      if n < 4 then out(opENVACC1 + n) else (out opENVACC; out_int (n+1))
  | Kpush ->
      out opPUSH
  | Kpop n ->
      out opPOP; out_int n
  | Kassign n ->
      out opASSIGN; out_int n
  | Kpush_retaddr lbl -> out opPUSH_RETADDR; out_label lbl
  | Kapply n ->
      if n < 4 then out(opAPPLY1 + n - 1) else (out opAPPLY; out_int n)
  | Kappterm(n, sz) ->
      if n < 4 then (out(opAPPTERM1 + n - 1); out_int sz)
               else (out opAPPTERM; out_int n; out_int sz)
  | Kreturn n -> out opRETURN; out_int n
  | Krestart -> out opRESTART
  | Kgrab n -> out opGRAB; out_int n
  | Kclosure(lbl, n) -> out opCLOSURE; out_int n; out_label lbl
  | Kclosurerec(lbl, n) -> out opCLOSUREREC; out_int n; out_label lbl
  | Kgetglobal q -> out opGETGLOBAL; slot_for_getglobal q
  | Ksetglobal q -> out opSETGLOBAL; slot_for_setglobal q
  | Kconst sc ->
      begin match sc with
        Const_base(Const_int i) when i >= immed_min & i <= immed_max ->
          if i >= 0 & i <= 3
          then out (opCONST0 + i)
          else (out opCONSTINT; out_int i)
      | Const_base(Const_char c) ->
          out opCONSTINT; out_int (Char.code c)
      | Const_pointer i ->
          if i >= 0 & i <= 3
          then out (opCONST0 + i)
          else (out opCONSTINT; out_int i)
      | Const_block(t, []) ->
          if t = 0 then out opATOM0 else (out opATOM; out_int t)
      | _ ->
          out opGETGLOBAL; slot_for_literal sc
      end
  | Kmakeblock(n, t) ->
      if n = 0 then
        if t < 4 then out (opATOM0 + t) else (out opATOM; out_int t)
      else if n < 4 then (out(opMAKEBLOCK1 + n - 1); out_int t)
      else (out opMAKEBLOCK; out_int n; out_int t)
  | Kgetfield n ->
      if n < 4 then out(opGETFIELD0 + n) else (out opGETFIELD; out_int n)
  | Ksetfield n ->
      if n < 4 then out(opSETFIELD0 + n) else (out opSETFIELD; out_int n)
  | Kdummy n ->
      if n = 0 then out opATOM0 else (out opDUMMY; out_int n)
  | Kupdate n -> out opUPDATE
  | Kvectlength -> out opVECTLENGTH
  | Kgetvectitem -> out opGETVECTITEM
  | Ksetvectitem -> out opSETVECTITEM
  | Kgetstringchar -> out opGETSTRINGCHAR
  | Ksetstringchar -> out opSETSTRINGCHAR
  | Kbranch lbl -> out opBRANCH; out_label lbl
  | Kbranchif lbl -> out opBRANCHIF; out_label lbl
  | Kbranchifnot lbl -> out opBRANCHIFNOT; out_label lbl
  | Kstrictbranchif lbl -> out opBRANCHIF; out_label lbl
  | Kstrictbranchifnot lbl -> out opBRANCHIFNOT; out_label lbl
  | Kswitch(tbl_const, tbl_block) ->
      out opSWITCH;
      out_int (Array.length tbl_const + (Array.length tbl_block lsl 16));
      let org = !out_position in
      Array.iter (out_label_with_orig org) tbl_const;
      Array.iter (out_label_with_orig org) tbl_block
  | Kboolnot -> out opBOOLNOT
  | Kpushtrap lbl -> out opPUSHTRAP; out_label lbl
  | Kpoptrap -> out opPOPTRAP
  | Kraise -> out opRAISE
  | Kcheck_signals -> out opCHECK_SIGNALS
  | Kccall(name, n) ->
      if n <= 5
      then (out (opC_CALL1 + n - 1); slot_for_c_prim name)
      else (out opC_CALLN; out_int n; slot_for_c_prim name)
  | Knegint -> out opNEGINT  | Kaddint -> out opADDINT
  | Ksubint -> out opSUBINT  | Kmulint -> out opMULINT
  | Kdivint -> out opDIVINT  | Kmodint -> out opMODINT
  | Kandint -> out opANDINT  | Korint -> out opORINT
  | Kxorint -> out opXORINT  | Klslint -> out opLSLINT
  | Klsrint -> out opLSRINT  | Kasrint -> out opASRINT
  | Kintcomp Ceq -> out opEQ         | Kintcomp Cneq -> out opNEQ
  | Kintcomp Clt -> out opLTINT      | Kintcomp Cle -> out opLEINT
  | Kintcomp Cgt -> out opGTINT      | Kintcomp Cge -> out opGEINT
  | Koffsetint n -> out opOFFSETINT; out_int n
  | Koffsetref n -> out opOFFSETREF; out_int n
  | Kisint -> out opISINT
  | Kevent ev -> record_event ev
  | Kstop -> out opSTOP
(*e: constant [[Emitcode.emit_instr]] *)

(*s: constant [[Emitcode.emit]] *)
(* Emission of a list of instructions. Include some peephole optimization. *)

let rec emit = function
    [] -> ()
  (* Peephole optimizations *)
  | Kpush :: Kacc n :: c ->
      if n < 8 then out(opPUSHACC0 + n) else (out opPUSHACC; out_int n);
      emit c
  | Kpush :: Kenvacc n :: c ->
      if n < 4 then out(opPUSHENVACC1 + n)
               else (out opPUSHENVACC; out_int (n+1));
      emit c
  | Kpush :: Kgetglobal id :: Kgetfield n :: c ->
      out opPUSHGETGLOBALFIELD; slot_for_getglobal id; out_int n; emit c
  | Kpush :: Kgetglobal id :: c ->
      out opPUSHGETGLOBAL; slot_for_getglobal id; emit c
  | Kpush :: Kconst sc :: c ->
      begin match sc with
        Const_base(Const_int i) when i >= immed_min & i <= immed_max ->
          if i >= 0 & i <= 3
          then out (opPUSHCONST0 + i)
          else (out opPUSHCONSTINT; out_int i)
      | Const_base(Const_char c) ->
          out opPUSHCONSTINT; out_int(Char.code c)
      | Const_pointer i ->
          if i >= 0 & i <= 3
          then out (opPUSHCONST0 + i)
          else (out opPUSHCONSTINT; out_int i)
      | Const_block(t, []) ->
          if t = 0 then out opPUSHATOM0 else (out opPUSHATOM; out_int t)
      | _ ->
          out opPUSHGETGLOBAL; slot_for_literal sc
      end;
      emit c
  | Kpush :: (Kevent {ev_kind = Event_before} as ev) ::
    (Kgetglobal _ as instr1) :: (Kgetfield _ as instr2) :: c ->
      emit (Kpush :: instr1 :: instr2 :: ev :: c)
  | Kpush :: (Kevent {ev_kind = Event_before} as ev) ::
    (Kacc _ | Kenvacc _ | Kgetglobal _ | Kconst _ as instr) :: c ->
      emit (Kpush :: instr :: ev :: c)
  | Kgetglobal id :: Kgetfield n :: c ->
      out opGETGLOBALFIELD; slot_for_getglobal id; out_int n; emit c
  (* Default case *)
  | instr :: c ->
      emit_instr instr; emit c
(*e: constant [[Emitcode.emit]] *)

(*s: function [[Emitcode.to_file]] *)
(* Emission to a file *)

let to_file outchan unit_name code =
  init();

  output_string outchan cmo_magic_number;
  let pos_depl = pos_out outchan in
  output_binary_int outchan 0;
  let pos_code = pos_out outchan in
  emit code;
  output outchan !out_buffer 0 !out_position;
  let (pos_debug, size_debug) =
    if !Clflags.debug then begin
      let p = pos_out outchan in
      output_value outchan !events;
      (p, pos_out outchan - p)
    end else
      (0, 0) 
  in
  let compunit =
    { cu_name = unit_name;
      cu_pos = pos_code;
      cu_codesize = !out_position;
      cu_reloc = List.rev !reloc_info;
      cu_imports = Env.imported_units();
      cu_primitives = !Translmod.primitive_declarations;
      cu_force_link = false;
      cu_debug = pos_debug;
      cu_debugsize = size_debug } in
  init();                               (* Free out_buffer and reloc_info *)
(*  Btype.cleanup_abbrev ();*)              (* Remove any cached abbreviation
                                           expansion before saving *)
  let pos_compunit = pos_out outchan in
  output_value outchan compunit;
  seek_out outchan pos_depl;
  output_binary_int outchan pos_compunit
(*e: function [[Emitcode.to_file]] *)

(*s: function [[Emitcode.to_memory]] *)
(* Emission to a memory block *)

let to_memory init_code fun_code =
  init();
  emit init_code;
  emit fun_code;
  let code = Meta.static_alloc !out_position in
  String.unsafe_blit (Bytes.to_string !out_buffer) 0 code 0 !out_position;
  let reloc = List.rev !reloc_info
  and code_size = !out_position in
  init();
  (code, code_size, reloc)
(*e: function [[Emitcode.to_memory]] *)
(*e: bytecomp/emitcode.ml *)
