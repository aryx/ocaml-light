(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Instruction selection for the Mips processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

open Selectgen

let selector () =
  let super = Selectgen.selector_generic () in
  { super with

  is_immediate = (fun (n : int) -> true);

  select_addressing = (function
      Cconst_symbol s ->
        (Ibased(s, 0), Ctuple [])
    | Cop(Cadda, [Cconst_symbol s; Cconst_int n]) ->
        (Ibased(s, n), Ctuple [])
    | Cop(Cadda, [arg; Cconst_int n]) ->
        (Iindexed n, arg)
    | Cop(Cadda, [arg1; Cop(Caddi, [arg2; Cconst_int n])]) ->
        (Iindexed n, Cop(Cadda, [arg1; arg2]))
    | arg ->
        (Iindexed 0, arg)
  );
  }

let fundecl f =
  let s = selector () in
  s.emit_fundecl s f
