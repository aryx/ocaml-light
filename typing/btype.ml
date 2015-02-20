(*s: ./typing/btype.ml *)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


(* Basic operations on core types *)

open Types

(*s: constant Btype.generic_level *)
(**** Type level management ****)

let generic_level = 100000000
(*e: constant Btype.generic_level *)

(*s: constant Btype.lowest_level *)
(* Used to mark a type during a traversal. *)
let lowest_level = 0
(*e: constant Btype.lowest_level *)
(*s: constant Btype.pivot_level *)
let pivot_level = 2 * lowest_level - 1
    (* pivot_level - lowest_level < lowest_level *)
(*e: constant Btype.pivot_level *)

(*s: function Btype.newgenty *)
(**** Some type creators ****)

let newgenty desc      = { desc = desc; level = generic_level }
(*e: function Btype.newgenty *)
(*s: function Btype.newgenvar *)
let newgenvar ()       = newgenty Tvar
(*e: function Btype.newgenvar *)
(*s: function Btype.newmarkedgenvar *)
let newmarkedgenvar () = { desc = Tvar; level = pivot_level - generic_level }
(*e: function Btype.newmarkedgenvar *)

(*s: constant Btype.repr *)
(**** Representative of a type ****)

let rec repr =
  function
    {desc = Tlink t'} ->
      (* 
         We do no path compression. Path compression does not seem to
         improve notably efficiency, and it prevents from changing a
         [Tlink] into another type (for instance, for undoing a
         unification).
      *)
      repr t'
  | t -> t
(*e: constant Btype.repr *)

                  (**********************************)
                  (*  Utilities for type traversal  *)
                  (**********************************)


(*s: function Btype.iter_type_expr *)
let iter_type_expr f ty =
  match ty.desc with
    Tvar               -> ()
  | Tarrow (ty1, ty2) -> f ty1; f ty2
  | Ttuple l           -> List.iter f l
  | Tconstr (_, l, _)          -> List.iter f l
  | Tnil               -> ()
  | Tlink ty           -> f ty
(*e: function Btype.iter_type_expr *)

(*s: constant Btype.saved_desc *)
let saved_desc = ref []
  (* Saved association of generic nodes with their description. *)
(*e: constant Btype.saved_desc *)

(*s: function Btype.save_desc *)
let save_desc ty desc = 
  saved_desc := (ty, desc)::!saved_desc
(*e: function Btype.save_desc *)

(*s: function Btype.cleanup_types *)
(* Restored type descriptions *)
let cleanup_types () =
  List.iter (fun (ty, desc) -> ty.desc <- desc) !saved_desc;
  saved_desc := []
(*e: function Btype.cleanup_types *)

(*s: function Btype.unmark_type *)
(* Remove marks from a type. *)
let rec unmark_type ty =
  let ty = repr ty in
  if ty.level < lowest_level then begin
    ty.level <- pivot_level - ty.level;
    iter_type_expr unmark_type ty
  end
(*e: function Btype.unmark_type *)


                  (*******************************************)
                  (*  Memorization of abbreviation expansion *)
                  (*******************************************)

(*s: constant Btype.memo *)
let memo = ref []
        (* Contains the list of saved abbreviation expansions. *)
(*e: constant Btype.memo *)

(*s: function Btype.cleanup_abbrev *)
let cleanup_abbrev () =
        (* Remove all memorized abbreviation expansions. *)
  List.iter (fun abbr -> abbr := Mnil) !memo;
  memo := []
(*e: function Btype.cleanup_abbrev *)

(*s: function Btype.memorize_abbrev *)
let memorize_abbrev mem path v =
        (* Memorize the expansion of an abbreviation. *)
  mem := Mcons (path, v, !mem);
  memo := mem :: !memo
(*e: function Btype.memorize_abbrev *)
(*e: ./typing/btype.ml *)
