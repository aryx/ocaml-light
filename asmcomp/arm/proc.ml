(*s: asmcomp/arm/proc.ml *)
(*s: copyright header 1998 *)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header 1998 *)

(* Description of the ARM processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(*s: constant [[Proc.word_addressed]] *)
(* Instruction selection *)

let word_addressed = false
(*e: constant [[Proc.word_addressed]] *)

(* Registers available for register allocation *)

(*s: constant [[Proc.int_reg_name]] *)
(* Register map:
    r0 - r7                     general purpose (r4 - r7 preserved by C)
    r8                          allocation pointer (preserved by C)
    r9                          allocation limit (preserved by C)
    r10                         general purpose
    r11                         trap pointer (preserved by C)
    r12                         general purpose
    r13                         stack pointer
    r14                         return address
    r15                         program counter

    f0 - f7                     general purpose (f4 - f7 preserved by C)
*)

let int_reg_name = [|
(*e: constant [[Proc.int_reg_name]] *)
  "r0"; "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r10"; "r12"
|]
  
(*s: constant [[Proc.float_reg_name]] *)
let float_reg_name = [|
(*e: constant [[Proc.float_reg_name]] *)
  "d0"; "d1"; "d2"; "d3"; "d4"; "d5"; "d6"; "d7"
|]

(*s: constant [[Proc.num_register_classes]] *)
let num_register_classes = 2
(*e: constant [[Proc.num_register_classes]] *)

(*s: function [[Proc.register_class]] *)
let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1
(*e: function [[Proc.register_class]] *)

(*s: constant [[Proc.num_available_registers]] *)
let num_available_registers = [| 10; 8 |]
(*e: constant [[Proc.num_available_registers]] *)

(*s: constant [[Proc.first_available_register]] *)
let first_available_register = [| 0; 100 |]
(*e: constant [[Proc.first_available_register]] *)

(*s: function [[Proc.register_name]] *)
let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)
(*e: function [[Proc.register_name]] *)

(*s: constant [[Proc.rotate_registers]] *)
let rotate_registers = true
(*e: constant [[Proc.rotate_registers]] *)

(*s: constant [[Proc.hard_int_reg]] *)
(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.create 10 Reg.dummy in
  for i = 0 to 9 do v.(i) <- Reg.at_location Int (Reg i) done;
  v
(*e: constant [[Proc.hard_int_reg]] *)

(*s: constant [[Proc.hard_float_reg]] *)
let hard_float_reg =
  let v = Array.create 8 Reg.dummy in
  for i = 0 to 7 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done;
  v
(*e: constant [[Proc.hard_float_reg]] *)

(*s: constant [[Proc.all_phys_regs]] *)
let all_phys_regs =
  Array.append hard_int_reg hard_float_reg
(*e: constant [[Proc.all_phys_regs]] *)

(*s: function [[Proc.phys_reg]] *)
let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)
(*e: function [[Proc.phys_reg]] *)

(*s: function [[Proc.stack_slot]] *)
let stack_slot slot ty =
  Reg.at_location ty (Stack slot)
(*e: function [[Proc.stack_slot]] *)

(*s: function [[Proc.calling_conventions]] *)
(* Calling conventions *)

let calling_conventions first_int last_int first_float last_float
                        make_stack arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg !int;
          incr int
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, !ofs)
(*e: function [[Proc.calling_conventions]] *)

(*s: function [[Proc.incoming]] *)
let incoming ofs = Incoming ofs
(*e: function [[Proc.incoming]] *)
(*s: function [[Proc.outgoing]] *)
let outgoing ofs = Outgoing ofs
(*e: function [[Proc.outgoing]] *)
(*s: function [[Proc.not_supported]] *)
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"
(*e: function [[Proc.not_supported]] *)

(*s: function [[Proc.loc_arguments]] *)
let loc_arguments arg =
  calling_conventions 0 7 100 103 outgoing arg
(*e: function [[Proc.loc_arguments]] *)
(*s: function [[Proc.loc_parameters]] *)
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 0 7 100 103 incoming arg in loc
(*e: function [[Proc.loc_parameters]] *)
(*s: function [[Proc.loc_results]] *)
let loc_results res =
  let (loc, ofs) = calling_conventions 0 7 100 103 not_supported res in loc
(*e: function [[Proc.loc_results]] *)

(*s: function [[Proc.loc_external_arguments]] *)
(* Calling conventions for C are as for Caml, except that float arguments
   are passed in pairs of integer registers. *)

let loc_external_arguments arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let reg = ref 0 in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !reg <= 3 then begin
          loc.(i) <- phys_reg !reg;
          incr reg
        end else begin
          loc.(i) <- stack_slot (outgoing !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !reg <= 2 then begin
          loc.(i) <- phys_reg !reg;
          reg := !reg + 2
        end else begin
          loc.(i) <- stack_slot (outgoing !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, !ofs)
(*e: function [[Proc.loc_external_arguments]] *)

(*s: function [[Proc.loc_external_results]] *)
let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 0 100 100 not_supported res in loc
(*e: function [[Proc.loc_external_results]] *)

(*s: constant [[Proc.loc_exn_bucket]] *)
let loc_exn_bucket = phys_reg 0
(*e: constant [[Proc.loc_exn_bucket]] *)

(*s: constant [[Proc.destroyed_at_c_call]] *)
(* Registers destroyed by operations *)

let destroyed_at_c_call =               (* r4-r9, f4-f7 preserved *)
  Array.of_list(List.map phys_reg [0;1;2;3;8;9; 100;101;102;103])
(*e: constant [[Proc.destroyed_at_c_call]] *)

(*s: function [[Proc.destroyed_at_oper]] *)
let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | Iop(Ialloc(_)) -> [|phys_reg 8|]	(* r10 destroyed *)
  | _ -> [||]
(*e: function [[Proc.destroyed_at_oper]] *)

(*s: constant [[Proc.destroyed_at_raise]] *)
let destroyed_at_raise = all_phys_regs
(*e: constant [[Proc.destroyed_at_raise]] *)

(*s: function [[Proc.safe_register_pressure]] *)
(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall(_, _) -> 4
  | _ -> 8
(*e: function [[Proc.safe_register_pressure]] *)
(*s: function [[Proc.max_register_pressure]] *)
let max_register_pressure = function
    Iextcall(_, _) -> [| 4; 4 |]
  | _ -> [| 10; 8 |]
(*e: function [[Proc.max_register_pressure]] *)

(*s: constant [[Proc.num_stack_slots]] *)
(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
(*e: constant [[Proc.num_stack_slots]] *)
(*s: constant [[Proc.contains_calls]] *)
let contains_calls = ref false
(*e: constant [[Proc.contains_calls]] *)

(*s: function [[Proc.assemble_file]] *)
(* Calling the assembler *)

let assemble_file infile outfile =
  (* TODO: need use Config.assembler or something!! *)
  let cmd = ("arm-linux-gnueabihf-as -g -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -o " ^ outfile ^ " " ^ infile) in
  Logs.info (fun m -> m "running %s" cmd);
  Sys.command cmd
(*e: function [[Proc.assemble_file]] *)

(*e: asmcomp/arm/proc.ml *)
