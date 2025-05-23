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


(* Handling of keyboard interrupts *)

let interrupted = ref false

let is_protected = ref false

let break signum =
  if !is_protected
  then interrupted := true
  else raise Sys.Break

let _ =
  Sys.signal Sys.sigint (Sys.Signal_handle break);
  Sys.signal Sys.sigpipe (Sys.Signal_handle (fun _ -> raise End_of_file))

let protect f =
  if !is_protected then
    f ()
  else begin
    is_protected := true;
    if not !interrupted then
       f ();
    is_protected := false;
    if !interrupted then begin interrupted := false; raise Sys.Break end
  end

let unprotect f =
  if not !is_protected then
    f ()
  else begin
    is_protected := false;
    if !interrupted then begin interrupted := false; raise Sys.Break end;
    f ();
    is_protected := true
  end
