(*s: ./typing/ident.ml *)
(*s: copyright header0 *)
(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header0 *)

open Format

(*s: type Ident.t *)
type t = { 
  mutable name: string; 
  mutable stamp: int; 
  (*s: [[Ident.t]] other fields *)
  mutable global: bool 
  (*e: [[Ident.t]] other fields *)
}
(*e: type Ident.t *)

(*s: constant Ident.currentstamp *)
(* A stamp of 0 denotes a persistent identifier *)
let currentstamp = ref 0
(*e: constant Ident.currentstamp *)

(*s: function Ident.create *)
let create s =
  incr currentstamp;
  { name = s; stamp = !currentstamp; global = false }
(*e: function Ident.create *)

(*s: function Ident.create_persistent *)
let create_persistent s =
  { name = s; stamp = 0; global = true }
(*e: function Ident.create_persistent *)

(*s: function Ident.name *)
let name i = 
  i.name
(*e: function Ident.name *)

(*s: function Ident.unique_name *)
let unique_name i = 
  i.name ^ "_" ^ string_of_int i.stamp
(*e: function Ident.unique_name *)

(*s: function Ident.persistent *)
let persistent i = 
  (i.stamp = 0)
(*e: function Ident.persistent *)

(*s: function Ident.equal *)
let equal i1 i2 = 
  i1.name = i2.name
(*e: function Ident.equal *)

(*s: function Ident.same *)
let same i1 i2 = 
  i1 = i2
(*e: function Ident.same *)

(*s: function Ident.identify *)
let identify i1 i2 f =
  let name1 = i1.name and stamp1 = i1.stamp in
  try
    i1.name <- i2.name;
    i1.stamp <- i2.stamp;
    let res = f () in
    i1.name <- name1;
    i1.stamp <- stamp1;
    res
  with x ->
    i1.name <- name1;
    i1.stamp <- stamp1;
    raise x
(*e: function Ident.identify *)

(*s: function Ident.hide *)
let hide i =
  { stamp = -1; name = i.name; global = i.global }
(*e: function Ident.hide *)

(*s: function Ident.make_global *)
let make_global i =
  i.global <- true
(*e: function Ident.make_global *)

(*s: function Ident.global *)
let global i =
  i.global
(*e: function Ident.global *)

(*s: function Ident.print *)
let print i =
  print_string i.name;
  match i.stamp with
    0 -> print_string "!"
  | -1 -> print_string "#"
  | n -> print_string "/"; print_int n; if i.global then print_string "g"
(*e: function Ident.print *)

(*s: type Ident.tbl *)
type 'a tbl =
    Empty
  | Node of 'a tbl * 'a data * 'a tbl * int
(*e: type Ident.tbl *)

(*s: type Ident.data *)
and 'a data =
  { ident: t;
    data: 'a;
    previous: 'a data option }
(*e: type Ident.data *)

(*s: constant Ident.empty *)
let empty = Empty
(*e: constant Ident.empty *)

(*s: function Ident.mknode *)
(* Inline expansion of height for better speed
 * let height = function
 *     Empty -> 0
 *   | Node(_,_,_,h) -> h
 *)

let mknode l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
  and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  Node(l, d, r, (if hl >= hr then hl + 1 else hr + 1))
(*e: function Ident.mknode *)

(*s: function Ident.balance *)
let balance l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
  and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 1 then
    let (Node(ll, ld, lr, _)) = l in
    if (match ll with Empty -> 0 | Node(_,_,_,h) -> h) >=
       (match lr with Empty -> 0 | Node(_,_,_,h) -> h) then
      mknode ll ld (mknode lr d r)
    else
      let (Node(lrl, lrd, lrr, _)) = lr in
      mknode (mknode ll ld lrl) lrd (mknode lrr d r)
  else if hr > hl + 1 then
    let (Node(rl, rd, rr, _)) = r in
    if (match rr with Empty -> 0 | Node(_,_,_,h) -> h) >=
       (match rl with Empty -> 0 | Node(_,_,_,h) -> h) then
      mknode (mknode l d rl) rd rr
    else
      let (Node(rll, rld, rlr, _)) = rl in
      mknode (mknode l d rll) rld (mknode rlr rd rr)
  else
    mknode l d r
(*e: function Ident.balance *)

(*s: function Ident.add *)
let rec add id data = function
    Empty ->
      Node(Empty, {ident = id; data = data; previous = None}, Empty, 1)
  | Node(l, k, r, h) ->
      let c = compare id.name k.ident.name in
      if c = 0 then
        Node(l, {ident = id; data = data; previous = Some k}, r, h)
      else if c < 0 then
        balance (add id data l) k r
      else
        balance l k (add id data r)
(*e: function Ident.add *)

(*s: function Ident.find_stamp *)
let rec find_stamp s = function
    None ->
      raise Not_found
  | Some k ->
      if k.ident.stamp = s then k.data else find_stamp s k.previous
(*e: function Ident.find_stamp *)

(*s: function Ident.find_same *)
let rec find_same id = function
    Empty ->
      raise Not_found
  | Node(l, k, r, _) ->
      let c = compare id.name k.ident.name in
      if c = 0 then
        if id.stamp = k.ident.stamp
        then k.data
        else find_stamp id.stamp k.previous
      else
        find_same id (if c < 0 then l else r)
(*e: function Ident.find_same *)

(*s: function Ident.find_name *)
let rec find_name name = function
    Empty ->
      raise Not_found
  | Node(l, k, r, _) ->
      let c = compare name k.ident.name in
      if c = 0 then
        k.data
      else
        find_name name (if c < 0 then l else r)
(*e: function Ident.find_name *)

(*s: function Ident.print_tbl *)
let print_tbl print_elt tbl =
  open_hovbox 2;
  print_string "[[";
  let rec print_tbl = function
      Empty -> ()
    | Node(l, k, r, _) ->
        print_tbl l;
        print_entry k;
        print_tbl r
  and print_entry k =
    open_hovbox 2;
    print k.ident; print_string " ->"; print_space(); print_elt k.data;
    print_string ";"; close_box(); print_space();
    match k.previous with None -> () | Some k -> print_entry k in
  print_tbl tbl;
  print_string "]]";
  close_box()
(*e: function Ident.print_tbl *)

(*e: ./typing/ident.ml *)
