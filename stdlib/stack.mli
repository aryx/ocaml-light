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


(* Module [Stack]: last-in first-out stacks *)

(* This module implements stacks (LIFOs), with in-place modification. *)

type 'a t
        (* The type of stacks containing elements of type ['a]. *)

exception Empty
        (* Raised when [pop] is applied to an empty stack. *)

val create: unit -> 'a t
        (* Return a new stack, initially empty. *)
val push: 'a -> 'a t -> unit
        (* [push x s] adds the element [x] at the top of stack [s]. *)
val pop: 'a t -> 'a
        (* [pop s] removes and returns the topmost element in stack [s],
           or raises [Empty] if the stack is empty. *)
val clear : 'a t -> unit
        (* Discard all elements from a stack. *)
val length: 'a t -> int
        (* Return the number of elements in a stack. *)
val iter: ('a -> unit) -> 'a t -> unit
        (* [iter f s] applies [f] in turn to all elements of [s],
           from the element at the top of the stack to the element at the
           bottom of the stack. The stack itself is unchanged. *)

val top: 'a t -> 'a

(* since 4.08 *)
val top_opt: 'a t -> 'a option

(* TODO: remove, not part of modern OCaml *)
val nth: int -> 'a t -> 'a
