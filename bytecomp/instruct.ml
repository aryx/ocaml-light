(*s: ./bytecomp/instruct.ml *)
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

open Lambda

(*s: type Instruct.compilation_env *)
(* Structure of compilation environments *)

type compilation_env =
  { ce_stack: int Ident.tbl; (* Positions of variables in the stack *)
    ce_heap: int Ident.tbl } (* Structure of the heap-allocated env *)
(*e: type Instruct.compilation_env *)


(*s: type Instruct.debug_event *)
type debug_event =
  { mutable ev_pos: int;                (* Position in bytecode *)
    ev_module: string;                  (* Name of defining module *)
    ev_char: int;                       (* Location in source file *)
    ev_kind: debug_event_kind;          (* Before/after event *)
    ev_info: debug_event_info;          (* Extra information *)
    ev_typenv: unit(*Env.summary*);             (* Typing environment *)
    ev_compenv: compilation_env;        (* Compilation environment *)
    ev_stacksize: int;                  (* Size of stack frame *)
    ev_repr: debug_event_repr }         (* Position of the representative *)
(*e: type Instruct.debug_event *)

(*s: type Instruct.debug_event_kind *)
and debug_event_kind =
    Event_before
  | Event_after of Types.type_expr
  | Event_pseudo
(*e: type Instruct.debug_event_kind *)

(*s: type Instruct.debug_event_info *)
and debug_event_info =
    Event_function
  | Event_return of int
  | Event_other
(*e: type Instruct.debug_event_info *)

(*s: type Instruct.debug_event_repr *)
and debug_event_repr =
    Event_none
  | Event_parent of int ref
  | Event_child of int ref
(*e: type Instruct.debug_event_repr *)

(*s: type Instruct.label *)
(* Abstract machine instructions *)

type label = int                        (* Symbolic code labels *)
(*e: type Instruct.label *)

(*s: type Instruct.instruction *)
type instruction =
    Klabel of label

  | Kacc of int
  | Kenvacc of int
  | Kpush
  | Kpop of int
  | Kassign of int

  | Kpush_retaddr of label
  | Kapply of int                       (* number of arguments *)
  | Kappterm of int * int               (* number of arguments, slot size *)
  | Kreturn of int                      (* slot size *)
  | Krestart
  | Kgrab of int                        (* number of arguments *)
  | Kclosure of label * int
  | Kclosurerec of label * int

  | Kgetglobal of Ident.t
  | Ksetglobal of Ident.t

  | Kconst of structured_constant
  | Kmakeblock of int * int             (* size, tag *)
  | Kgetfield of int
  | Ksetfield of int

  | Kdummy of int                       (* block size *)
  | Kupdate of int                      (* block size *)

  | Kvectlength
  | Kgetvectitem
  | Ksetvectitem

  | Kgetstringchar
  | Ksetstringchar

  | Kbranch of label
  | Kbranchif of label
  | Kbranchifnot of label
  | Kstrictbranchif of label
  | Kstrictbranchifnot of label
  | Kswitch of label array * label array
  | Kboolnot

  | Kpushtrap of label
  | Kpoptrap
  | Kraise
  | Kcheck_signals

  | Kccall of string * int

  | Knegint | Kaddint | Ksubint | Kmulint | Kdivint | Kmodint
  | Kandint | Korint | Kxorint | Klslint | Klsrint | Kasrint
  | Kintcomp of comparison

  | Koffsetint of int
  | Koffsetref of int

  | Kevent of debug_event
  | Kstop
(*e: type Instruct.instruction *)

let immed_min = -0x40000000
and immed_max = 0x3FFFFFFF

(* Actually the abstract machine accomodates -0x80000000 to 0x7FFFFFFF,
   but these numbers overflow the Caml type int if the compiler runs on
   a 32-bit processor. *)
(*e: ./bytecomp/instruct.ml *)
