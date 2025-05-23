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


(***************************** Input control ***************************)

open Primitives

(*** Actives files. ***)

(* Add a file to the list of active files. *)
val add_file : io_channel -> (io_channel -> unit) -> unit

(* Remove a file from the list of actives files. *)
val remove_file : io_channel -> unit

(* Return the controller currently attached to the given file. *)
val current_controller : io_channel -> (io_channel -> unit)

(* Execute a function with `controller' attached to `file'. *)
(* ### controller file funct *)
val execute_with_other_controller :
  (io_channel -> unit) -> io_channel -> (unit -> 'a) -> 'a

(*** The "Main Loop" ***)

(* Call this function for exiting the main loop. *)
val exit_main_loop : 'a -> unit

(* Handle active files until `continue_main_loop' is false. *)
val main_loop : unit -> unit

(*** Managing user inputs ***)

(* Are we in interactive mode ? *)
val interactif : bool ref

val current_prompt : string ref

(* Where the user input come from. *)
val user_channel : io_channel ref

val read_user_input : string -> int -> int

(* Stop reading user input. *)
val stop_user_input : unit -> unit

(* Resume reading user input. *)
val resume_user_input : unit -> unit

(* Ask user a yes or no question. *)
val yes_or_no : string -> bool
