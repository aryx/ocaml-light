(*s: yacc2/first_follow.mli *)

(*s: enum First_follow.first (yacc) *)
type first = (Ast.symbol, Ast.term Set.t) Map.t
(*e: enum First_follow.first (yacc) *)

(*s: enum First_follow.epsilon (yacc) *)
type epsilon = Ast.nonterm Set.t
(*e: enum First_follow.epsilon (yacc) *)

(*s: signature First_follow.compute_first (yacc) *)
val compute_first: Ast.grammar -> first * epsilon
(*e: signature First_follow.compute_first (yacc) *)

(*s: enum First_follow.follow (yacc) *)
type follow = (Ast.nonterm, Ast.term Set.t) Map.t
(*e: enum First_follow.follow (yacc) *)

(*s: signature First_follow.compute_follow (yacc) *)
(* take Lr0.env for its augmented grammar *)
val compute_follow: Lr0.env -> first * epsilon -> follow
(*e: signature First_follow.compute_follow (yacc) *)
(*e: yacc2/first_follow.mli *)
