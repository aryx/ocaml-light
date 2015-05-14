open Ast

let rec eval x =
  match x with
  | Int x -> x
  | Plus (x,y) -> eval x + eval y
  | Minus (x,y) -> eval x - eval y
  | Mult (x,y) -> eval x * eval y
  | Div (x,y) -> eval x / eval y
