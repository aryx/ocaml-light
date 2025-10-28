module Chan = struct
  type t = { ic : in_channel; origin: string }
end

let foo (x : Chan.t) =
  x.ic
