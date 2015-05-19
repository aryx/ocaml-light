
type first = (Ast.symbol, Ast.term Set.t) Map.t

type epsilon = Ast.nonterm Set.t

val compute_first: Ast.grammar -> first * epsilon

type follow = (Ast.nonterm, Ast.term Set.t) Map.t

(* take Lr0.env for its augmented grammar *)
val compute_follow: Lr0.env -> first * epsilon -> follow
