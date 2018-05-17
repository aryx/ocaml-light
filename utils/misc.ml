(*s: ./utils/misc.ml *)
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

(* Errors *)

(*s: exception [[Misc.Fatal_error]] *)
exception Fatal_error
(*e: exception [[Misc.Fatal_error]] *)

(*s: function [[Misc.fatal_error]] *)
let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Fatal_error
(*e: function [[Misc.fatal_error]] *)

(* List functions *)

(*s: function [[Misc.map_end]] *)
let rec map_end f l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl -> f hd :: map_end f tl l2
(*e: function [[Misc.map_end]] *)

(*s: function [[Misc.for_all2]] *)
let rec for_all2 pred l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (hd1::tl1, hd2::tl2) -> pred hd1 hd2 & for_all2 pred tl1 tl2
  | (_, _) -> false
(*e: function [[Misc.for_all2]] *)

(*s: function [[Misc.filter]] *)
let rec filter pred =
  function
    [] ->
      []
  | a::l ->
      if pred a then
        a::(filter pred l)
      else
        filter pred l
(*e: function [[Misc.filter]] *)

(*s: function [[Misc.mem_assq]] *)
let rec mem_assq x = function
    [] -> false
  | (a,b)::l -> a == x or mem_assq x l
(*e: function [[Misc.mem_assq]] *)

(*s: function [[Misc.replicate_list]] *)
let rec replicate_list elem n =
  if n <= 0 then [] else elem :: replicate_list elem (n-1)
(*e: function [[Misc.replicate_list]] *)

(* File functions *)

(*s: function [[Misc.find_in_path]] *)
let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end
(*e: function [[Misc.find_in_path]] *)

(*s: function [[Misc.remove_file]] *)
let remove_file filename =
  try
    Sys.remove filename
  with Sys_error msg ->
    ()
(*e: function [[Misc.remove_file]] *)

(* Hashtable functions *)

(*s: function [[Misc.create_hashtable]] *)
let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl
(*e: function [[Misc.create_hashtable]] *)

(*s: function [[Misc.copy_file]] *)
let copy_file ic oc =
  let buff = String.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()
(*e: function [[Misc.copy_file]] *)

(*s: function [[Misc.copy_file_chunk]] *)
let copy_file_chunk ic oc len =
  let buff = String.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len
(*e: function [[Misc.copy_file_chunk]] *)

(* Integer operations *)

(*s: function [[Misc.log2]] *)
let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)
(*e: function [[Misc.log2]] *)

(*s: function [[Misc.align]] *)
(* @Scheck: dead by nice to have *)
let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)
(*e: function [[Misc.align]] *)

(*s: function [[Misc.no_overflow_add]] *)
let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0
(*e: function [[Misc.no_overflow_add]] *)

(*s: function [[Misc.no_overflow_sub]] *)
let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0
(*e: function [[Misc.no_overflow_sub]] *)

(*e: ./utils/misc.ml *)
