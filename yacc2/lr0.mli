
(* the index of the rule in env.g *)
type ruleidx = R of int
(* the dot position in the rhs of a rule *)
type dotidx = D of int

type stateid = S of int

(* as mentionned in the dragon book *)
type item = ruleidx * dotidx

(* a.k.a an LR0 state *)
type items = item Set_poly.t

type env = {
  (* augmented grammar where r0 is $S -> start_original_grammar *)
  g: Ast.rule_ array;
}

type automaton = {
  states: items Set_poly.t;
  (* state 0 is the starting state *)
  int_to_state: items array;
  state_to_int: (items, stateid) Map_poly.t;
  (* goto mapping *)
  trans: (items * Ast.symbol, items) Map_poly.t;
}

val mk_env_augmented_grammar: Ast.nonterm (* start *) -> Ast.grammar -> env

val closure: env -> items -> items

val goto: env -> items -> Ast.symbol -> items

(* assumes augmented grammar *)
val canonical_lr0_automaton: env -> automaton


(* helper functions used also by slr.ml *)
val after_dot: Ast.rule_ -> dotidx -> Ast.symbol option

