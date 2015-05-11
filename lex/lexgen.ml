(*s: lex/lexgen.ml *)
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

(* Compiling a lexer definition *)

open Syntax

(*s: type Lexgen.regexp *)
(* Deep abstract syntax for regular expressions *)

type regexp =
    Empty
  | Chars of int
  | Action of int
  | Seq of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp
(*e: type Lexgen.regexp *)

(*s: type Lexgen.lexer_entry *)
type lexer_entry =
  { lex_name: string;
    lex_regexp: regexp;
    lex_actions: (int * location) list }
(*e: type Lexgen.lexer_entry *)
    
(* Representation of automata *)

(*s: type Lexgen.automata *)
type automata =
    Perform of int
  | Shift of automata_trans * automata_move array
(*e: type Lexgen.automata *)
(*s: type Lexgen.automata_trans *)
and automata_trans =
    No_remember
  | Remember of int
(*e: type Lexgen.automata_trans *)
(*s: type Lexgen.automata_move *)
and automata_move =
    Backtrack
  | Goto of int
(*e: type Lexgen.automata_move *)

(*s: type Lexgen.automata_entry *)
(* Representation of entry points *)

type automata_entry =
  { auto_name: string;
    auto_initial_state: int;
    auto_actions: (int * Syntax.location) list }
(*e: type Lexgen.automata_entry *)
    
(*s: constant Lexgen.chars *)
(* From shallow to deep syntax *)

let chars = ref ([] : int list list)
(*e: constant Lexgen.chars *)
(*s: constant Lexgen.chars_count *)
let chars_count = ref 0
(*e: constant Lexgen.chars_count *)
(*s: constant Lexgen.actions *)
let actions = ref ([] : (int * location) list)
(*e: constant Lexgen.actions *)
(*s: constant Lexgen.actions_count *)
let actions_count = ref 0
(*e: constant Lexgen.actions_count *)

(*s: function Lexgen.encode_regexp *)
let rec encode_regexp = function
    Epsilon -> Empty
  | Characters cl ->
      let n = !chars_count in
      chars := cl :: !chars;
      incr chars_count;
      Chars(n)
  | Sequence(r1,r2) ->
      Seq(encode_regexp r1, encode_regexp r2)
  | Alternative(r1,r2) ->
      Alt(encode_regexp r1, encode_regexp r2)
  | Repetition r ->
      Star (encode_regexp r)
(*e: function Lexgen.encode_regexp *)

(*s: function Lexgen.encode_casedef *)
let encode_casedef casedef =
  List.fold_left
   (fun reg (expr, act) ->
     let act_num = !actions_count in
     incr actions_count;
     actions := (act_num, act) :: !actions;
     Alt(reg, Seq(encode_regexp expr, Action act_num)))
   Empty
   casedef
(*e: function Lexgen.encode_casedef *)

(*s: function Lexgen.encode_lexdef *)
let encode_lexdef def =
  chars := [];
  chars_count := 0;
  let entry_list =
    List.map
      (fun (entry_name, casedef) ->
        actions := [];
        actions_count := 0;
        let re = encode_casedef casedef in
        { lex_name = entry_name;
          lex_regexp = re;
          lex_actions = List.rev !actions })
      def.entrypoints in
  let chr = Array.of_list (List.rev !chars) in
  chars := [];
  actions := [];
  (chr, entry_list)
(*e: function Lexgen.encode_lexdef *)

(*s: type Lexgen.transition *)
(* To generate directly a NFA from a regular expression.
   Confer Aho-Sethi-Ullman, dragon book, chap. 3 *)

type transition =
    OnChars of int
  | ToAction of int
(*e: type Lexgen.transition *)

module TransSet = Set

(*s: function Lexgen.nullable *)
let rec nullable = function
    Empty      -> true
  | Chars _    -> false
  | Action _   -> false
  | Seq(r1,r2) -> nullable r1 & nullable r2
  | Alt(r1,r2) -> nullable r1 or nullable r2
  | Star r     -> true
(*e: function Lexgen.nullable *)

(*s: function Lexgen.firstpos *)
let rec firstpos = function
    Empty      -> TransSet.empty
  | Chars pos  -> TransSet.add (OnChars pos) TransSet.empty
  | Action act -> TransSet.add (ToAction act) TransSet.empty
  | Seq(r1,r2) -> if nullable r1
                  then TransSet.union (firstpos r1) (firstpos r2)
                  else firstpos r1
  | Alt(r1,r2) -> TransSet.union (firstpos r1) (firstpos r2)
  | Star r     -> firstpos r
(*e: function Lexgen.firstpos *)

(*s: function Lexgen.lastpos *)
let rec lastpos = function
    Empty      -> TransSet.empty
  | Chars pos  -> TransSet.add (OnChars pos) TransSet.empty
  | Action act -> TransSet.add (ToAction act) TransSet.empty
  | Seq(r1,r2) -> if nullable r2
                  then TransSet.union (lastpos r1) (lastpos r2)
                  else lastpos r2
  | Alt(r1,r2) -> TransSet.union (lastpos r1) (lastpos r2)
  | Star r     -> lastpos r
(*e: function Lexgen.lastpos *)

(*s: function Lexgen.followpos *)
let followpos size entry_list =
  let v = Array.create size TransSet.empty in
  let fill_pos first = function
      OnChars pos -> v.(pos) <- TransSet.union first v.(pos)
    | ToAction _  -> () in
  let rec fill = function
      Seq(r1,r2) ->
        fill r1; fill r2;
        TransSet.iter (fill_pos (firstpos r2)) (lastpos r1)
    | Alt(r1,r2) ->
        fill r1; fill r2
    | Star r ->
        fill r;
        TransSet.iter (fill_pos (firstpos r)) (lastpos r)
    | _ -> () in
  List.iter (fun entry -> fill entry.lex_regexp) entry_list;
  v
(*e: function Lexgen.followpos *)

(*s: constant Lexgen.no_action *)
let no_action = max_int
(*e: constant Lexgen.no_action *)

(*s: function Lexgen.split_trans_set *)
let split_trans_set trans_set =
  TransSet.fold
    (fun trans (act, pos_set as act_pos_set) ->
      match trans with
        OnChars pos -> (act, pos :: pos_set)
      | ToAction act1 -> if act1 < act then (act1, pos_set) else act_pos_set)
    trans_set
    (no_action, [])
(*e: function Lexgen.split_trans_set *)

module StateMap = Map

(*s: constant Lexgen.state_map *)
let state_map = ref (StateMap.empty: (transition Set.t, int) StateMap.t)
(*e: constant Lexgen.state_map *)
(*s: constant Lexgen.todo *)
let todo = (Stack.create() : (transition Set.t * int) Stack.t)
(*e: constant Lexgen.todo *)
(*s: constant Lexgen.next_state_num *)
let next_state_num = ref 0
(*e: constant Lexgen.next_state_num *)

(*s: function Lexgen.reset_state_mem *)
let reset_state_mem () =
  state_map := StateMap.empty;
  Stack.clear todo;
  next_state_num := 0
(*e: function Lexgen.reset_state_mem *)

(*s: function Lexgen.get_state *)
let get_state st = 
  try
    StateMap.find st !state_map
  with Not_found ->
    let num = !next_state_num in
    incr next_state_num;
    state_map := StateMap.add st num !state_map;
    Stack.push (st, num) todo;
    num
(*e: function Lexgen.get_state *)

(*s: function Lexgen.map_on_all_states *)
let map_on_all_states f =
  let res = ref [] in
  begin try
    while true do
      let (st, i) = Stack.pop todo in
      let r = f st in
      res := (r, i) :: !res
    done
  with Stack.Empty -> ()
  end;
  !res
(*e: function Lexgen.map_on_all_states *)

(*s: function Lexgen.goto_state *)
let goto_state st =
  if TransSet.is_empty st then Backtrack else Goto (get_state st)
(*e: function Lexgen.goto_state *)

(*s: function Lexgen.transition_from *)
let transition_from chars follow pos_set = 
  let tr = Array.create 257 TransSet.empty in
  let shift = Array.create 257 Backtrack in
    List.iter
      (fun pos ->
        List.iter
          (fun c ->
             tr.(c) <- TransSet.union tr.(c) follow.(pos))
          chars.(pos))
      pos_set;
    for i = 0 to 256 do
      shift.(i) <- goto_state tr.(i)
    done;
    shift
(*e: function Lexgen.transition_from *)

(*s: function Lexgen.translate_state *)
let translate_state chars follow state =
  match split_trans_set state with
    (n, []) -> Perform n
  | (n, ps) -> Shift((if n = no_action then No_remember else Remember n),
                     transition_from chars follow ps)
(*e: function Lexgen.translate_state *)

(*s: function Lexgen.make_dfa *)
let make_dfa lexdef =
  let (chars, entry_list) = encode_lexdef lexdef in
  let follow = followpos (Array.length chars) entry_list in
  reset_state_mem();
  let initial_states =
    List.map
      (fun le ->
        { auto_name = le.lex_name;
          auto_initial_state = get_state(firstpos le.lex_regexp);
          auto_actions = le.lex_actions })
      entry_list in
  let states = map_on_all_states (translate_state chars follow) in
  let actions = Array.create !next_state_num (Perform 0) in
  List.iter (fun (act, i) -> actions.(i) <- act) states;
  reset_state_mem();
  (initial_states, actions)
(*e: function Lexgen.make_dfa *)
(*e: lex/lexgen.ml *)
