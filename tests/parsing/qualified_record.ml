
module M = struct
  type foo = {
    x : int;
    y : int;
  }
end

let foo = M.{ x = 1; y = 1 }
