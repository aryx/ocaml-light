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

(* Description of the Power PC *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Integer register map:
    0                   temporary, null register for some operations
    1                   stack pointer
    2                   pointer to table of contents
    3 - 10              function arguments and results
    11 - 12             temporaries
    13                  pointer to small data area
    14 - 28             general purpose, preserved by C
    29                  trap pointer
    30                  allocation limit
    31                  allocation pointer
  Floating-point register map:
    0                   temporary
    1 - 13              function arguments and results
    14 - 31             general purpose, preserved by C
*)

(* claude: the original ocaml sources also had a Rhapsody/Darwin variant
   of these arrays ("r3"/"f1"-style names, vs. plain numbers here) --
   dead code for ocaml-light's -target-arch power (elf/Linux only), so
   dropped; see the comment on Arch.toc. *)
let int_reg_name =
    [| "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10";
       "14"; "15"; "16"; "17"; "18"; "19"; "20"; "21";
       "22"; "23"; "24"; "25"; "26"; "27"; "28" |]

let float_reg_name =
    [| "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8";
       "9"; "10"; "11"; "12"; "13"; "14"; "15"; "16";
       "17"; "18"; "19"; "20"; "21"; "22"; "23"; "24";
       "25"; "26"; "27"; "28"; "29"; "30"; "31" |]

let num_register_classes = 2

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 23; 31 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.create 24 Reg.dummy in
  for i = 0 to 23 do v.(i) <- Reg.at_location Int (Reg i) done; v

let hard_float_reg =
  let v = Array.create 31 Reg.dummy in
  for i = 0 to 30 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done; v

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let calling_conventions
    first_int last_int first_float last_float make_stack stack_ofs arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref stack_ofs in
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
  let final_ofs = if toc && !ofs > 0 then !ofs + 24 else !ofs in
  (loc, Misc.align final_ofs 8)
  (* Keep stack 8-aligned. 
     Under PowerOpen, keep a free 24 byte linkage area at the bottom
     if we need to stack-allocate some arguments. *)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 0 7 100 112 outgoing 0 arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 0 7 100 112 incoming 0 arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 7 100 112 not_supported 0 res in loc

(* C calling conventions under SVR4:
     use GPR 3-10 and FPR 1-8 just like ML calling conventions.
     Using a float register does not affect the int registers.
     Always reserve 8 bytes at bottom of stack, plus whatever is needed
     to hold the overflow arguments. *)

(* claude: the original ocaml sources also had poweropen_external_conventions,
   a PowerOpen (AIX/Rhapsody) calling convention where a float argument
   reserves matching int registers too, for non-prototyped C functions --
   dead code for ocaml-light's -target-arch power (elf/Linux only, whose
   convention is the plain calling_conventions below), so dropped; see
   the comment on Arch.toc. *)
let loc_external_arguments = calling_conventions 0 7 100 107 outgoing 8

let extcall_use_push = false

(* Results are in GPR 3 and FPR 1 *)

let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 0 100 100 not_supported 0 res in loc

(* Exceptions are in GPR 3 *)

let loc_exn_bucket = phys_reg 0

(* Registers destroyed by operations *)

let destroyed_at_c_call =
  Array.of_list(List.map phys_reg
    [0; 1; 2; 3; 4; 5; 6; 7;
     100; 101; 102; 103; 104; 105; 106; 107; 108; 109; 110; 111; 112])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall(_, _) -> 15
  | _ -> 23

let max_register_pressure = function
    Iextcall(_, _) -> [| 15; 18 |]
  | _ -> [| 23; 30 |]

(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

(* claude: the original ocaml sources also had "aix" (as -u -m ppc/pwr)
   and "rhapsody" (plain as, no -u/-m) variants here -- dead code for
   ocaml-light's -target-arch power (elf/Linux only), so dropped; see
   the comment on Arch.toc. Goes through Config.asm/Config.asm_flags
   (configure's `as -u -m ppc`, see there) like every other backend,
   instead of a hardcoded command. *)
let assemble_file infile outfile =
  Ccomp.command
    (Config.asm ^ " " ^ Config.asm_flags ^ " -o " ^ outfile ^ " " ^ infile)
