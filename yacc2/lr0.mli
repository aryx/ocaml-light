
(* the index of the rule in env.g *)
type ruleidx = R of int
(* the dot position in the rhs of a rule *)
type dotidx = D of int

(* as mentionned in the dragon book *)
type item = ruleidx * dotidx

(* a.k.a an LR0 state *)
type items = item Set_poly.t

type env = {
  g: Ast.rule_ array;
}


val closure: env -> items -> items
