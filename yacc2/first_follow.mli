
type first = (Ast.symbol, Ast.term Set_poly.t) Map_poly.t

type epsilon = Ast.nonterm Set_poly.t

val compute_first: Ast.grammar -> first * epsilon

type follow = (Ast.nonterm, Ast.term Set_poly.t) Map_poly.t

(* take Lr0.env for its augmented grammar *)
val compute_follow: Lr0.env -> first * epsilon -> follow
