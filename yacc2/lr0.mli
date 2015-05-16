
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
  (* augmented grammar where r0 is e' -> start_original_grammar *)
  g: Ast.rule_ array;
}

type transitions = (items * Ast.symbol, items) Map_poly.t

type automaton = {
  states: items Set_poly.t;
  int_to_state: items array;
  state_to_int: (items, stateid) Map_poly.t;
  trans: transitions;
}

val closure: env -> items -> items

val goto: env -> items -> Ast.symbol -> items

val canonical_lr0_automaton: env -> automaton



