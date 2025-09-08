type foo = int
[@@whatever]

type foo2 = A | B [@@deriving show]

let bar () =
  2
[@@whatever]


module X = String
[@@whatever]
