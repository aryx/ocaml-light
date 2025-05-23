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


(* Description of the Intel 386 processor *)

open Misc
open Arch
open Format
open Cmm
open Reg
open Mach

(* Registers available for register allocation *)

(* Register map:
    eax         0               eax - edi: function arguments and results
    ebx         1               eax: C function results
    ecx         2               ebx, esi, edi, ebp: preserved by C
    edx         3
    esi         4
    edi         5
    ebp         6

    tos         100             top of floating-point stack. *)

let int_reg_name =
  [| "%eax"; "%ebx"; "%ecx"; "%edx"; "%esi"; "%edi"; "%ebp" |]

let float_reg_name =
  [| "%tos" |]

let num_register_classes = 2

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 7; 0 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

(* There is little scheduling, and some operations are more compact
   when their argument is %eax. *)

let rotate_registers = false

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.create 7 Reg.dummy in
  for i = 0 to 6 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let hard_float_reg = [| Reg.at_location Float (Reg 100) |]

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let eax = phys_reg 0
let ecx = phys_reg 2
let edx = phys_reg 3
let tos = phys_reg 100

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Instruction selection *)

let word_addressed = false

(* Calling conventions *)

let calling_conventions first_int last_int first_float last_float make_stack
                        arg =
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

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 0 5 100 99 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 0 5 100 99 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 5 100 100 not_supported res in loc
let extcall_use_push = true
let loc_external_arguments arg =
  fatal_error "Proc.loc_external_arguments"
let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 0 100 100 not_supported res in loc

let loc_exn_bucket = eax

(* Registers destroyed by operations *)

let destroyed_at_c_call =               (* ebx, esi, edi, ebp preserved *)
  [|eax; ecx; edx|]

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | Iop(Iintop(Idiv | Imod)) -> [| eax; edx |]
  | Iop(Iintop_imm(Imod, _)) -> [| eax |]
  | Iop(Ialloc _) -> [| eax |]
  | Iop(Iintop(Icomp _) | Iintop_imm(Icomp _, _)) -> [| eax |]
  | Iop(Iintoffloat) -> [| eax |]
  | Iifthenelse(Ifloattest(_, _), _, _) -> [| eax |]
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure op = 4

let max_register_pressure = function
    Iextcall(_, _) -> [| 4; max_int |]
  | Iintop(Idiv | Imod) -> [| 5; max_int |]
  | Ialloc _ | Iintop(Icomp _) | Iintop_imm(Icomp _, _) |
    Iintoffloat -> [| 6; max_int |]
  | _ -> [|7; max_int |]

(* Layout of the stack frame *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  (* TODO? the --32 should be a configurable parameter set in config.ml
   * by configure
   *)
  Ccomp.command ("as --32 -g -o " ^ outfile ^ " " ^ infile)

