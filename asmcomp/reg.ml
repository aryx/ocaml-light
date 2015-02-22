(*s: asmcomp/reg.ml *)
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

open Cmm

(*s: type Reg.t (asmcomp/reg.ml) *)
type t =
  { mutable name: string;
    stamp: int;
    typ: Cmm.machtype_component;
    mutable loc: location;
    mutable spill: bool;
    mutable interf: t list;
    mutable prefer: (t * int) list;
    mutable degree: int;
    mutable spill_cost: int;
    mutable visited: bool }
(*e: type Reg.t (asmcomp/reg.ml) *)

(*s: type Reg.location (asmcomp/reg.ml) *)
and location =
    Unknown
  | Reg of int
  | Stack of stack_location
(*e: type Reg.location (asmcomp/reg.ml) *)

(*s: type Reg.stack_location (asmcomp/reg.ml) *)
and stack_location =
    Local of int
  | Incoming of int
  | Outgoing of int
(*e: type Reg.stack_location (asmcomp/reg.ml) *)

(*s: type Reg.reg *)
type reg = t
(*e: type Reg.reg *)

(*s: constant Reg.dummy *)
let dummy =
  { name = ""; stamp = 0; typ = Int; loc = Unknown; spill = false;
    interf = []; prefer = []; degree = 0; spill_cost = 0; visited = false }
(*e: constant Reg.dummy *)

(*s: constant Reg.currstamp *)
let currstamp = ref 0
(*e: constant Reg.currstamp *)
(*s: constant Reg.reg_list *)
let reg_list = ref([] : t list)
(*e: constant Reg.reg_list *)

(*s: function Reg.create *)
let create ty =
  let r = { name = ""; stamp = !currstamp; typ = ty; loc = Unknown;
            spill = false; interf = []; prefer = []; degree = 0;
            spill_cost = 0; visited = false } in
  reg_list := r :: !reg_list;
  incr currstamp;
  r
(*e: function Reg.create *)

(*s: function Reg.createv *)
let createv tyv =
  let n = Array.length tyv in
  let rv = Array.create n dummy in
  for i = 0 to n-1 do rv.(i) <- create tyv.(i) done;
  rv
(*e: function Reg.createv *)

(*s: function Reg.clone *)
let clone r =
  let nr = create r.typ in
  nr.name <- r.name;
  nr
(*e: function Reg.clone *)

(*s: function Reg.at_location *)
let at_location ty loc =
  let r = { name = "R"; stamp = !currstamp; typ = ty; loc = loc; spill = false;
            interf = []; prefer = []; degree = 0; spill_cost = 0;
            visited = false } in
  incr currstamp;
  r
(*e: function Reg.at_location *)

(*s: function Reg.reset *)
let reset() = currstamp := 100; reg_list := []
(*e: function Reg.reset *)
(*s: function Reg.all_registers *)
let all_registers() = !reg_list
(*e: function Reg.all_registers *)
(*s: function Reg.num_registers *)
let num_registers() = !currstamp
(*e: function Reg.num_registers *)

(*s: function Reg.reinit_reg *)
let reinit_reg r =
  r.loc <- Unknown;
  r.interf <- [];
  r.prefer <- [];
  r.degree <- 0;
  (* Preserve the very high spill costs introduced by the reloading pass *)
  if r.spill_cost >= 100000
  then r.spill_cost <- 100000
  else r.spill_cost <- 0
(*e: function Reg.reinit_reg *)

(*s: function Reg.reinit *)
let reinit() =
  List.iter reinit_reg !reg_list
(*e: function Reg.reinit *)

(*s: function Reg.add_set_array *)
(*
TODO good enough?
module RegOrder =
  struct
    type t = reg
    let compare r1 r2 = r1.stamp - r2.stamp
  end
module Set = Set.Make(RegOrder)
module Map = Map.Make(RegOrder)
*)


let add_set_array s v =
  match Array.length v with
    0 -> s
  | 1 -> Set.add v.(0) s
  | n -> let rec add_all i =
           if i >= n then s else Set.add v.(i) (add_all(i+1))
         in add_all 0
(*e: function Reg.add_set_array *)

(*s: function Reg.diff_set_array *)
let diff_set_array s v =
  match Array.length v with
    0 -> s
  | 1 -> Set.remove v.(0) s
  | n -> let rec remove_all i =
           if i >= n then s else Set.remove v.(i) (remove_all(i+1))
         in remove_all 0
(*e: function Reg.diff_set_array *)

(*s: function Reg.inter_set_array *)
let inter_set_array s v =
  match Array.length v with
    0 -> Set.empty
  | 1 -> if Set.mem v.(0) s
         then Set.add v.(0) Set.empty
         else Set.empty
  | n -> let rec inter_all i =
           if i >= n then Set.empty
           else if Set.mem v.(i) s then Set.add v.(i) (inter_all(i+1))
           else inter_all(i+1)
         in inter_all 0
(*e: function Reg.inter_set_array *)

(*s: function Reg.set_of_array *)
let set_of_array v =
  match Array.length v with
    0 -> Set.empty
  | 1 -> Set.add v.(0) Set.empty
  | n -> let rec add_all i =
           if i >= n then Set.empty else Set.add v.(i) (add_all(i+1))
         in add_all 0
(*e: function Reg.set_of_array *)
(*e: asmcomp/reg.ml *)
