(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*    Copyright 2013 Institut National de Recherche en Informatique et *)
(*    en Automatique.  All rights reserved.  This file is distributed  *)
(*    under the terms of the Q Public License version 1.0.             *)
(*                                                                     *)
(***********************************************************************)

(* Reloading for the ARM 64 bits *)

open Reloadgen

let fundecl f =
  let r = Reloadgen.reload_generic () in
  r.fundecl r f
