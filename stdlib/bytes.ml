(* partial port of 4.02 bytes.ml just enough to compile some of ocaml-light with
 * dune and a recent OCaml (as well as xix)
 *)
type t = string

let length x = String.length x

let create x = String.create x

let to_string x = x

let of_string x = x

let get x n = String.get x n

let unsafe_get x n = String.unsafe_get x n
