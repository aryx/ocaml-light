
let no_caps : <Cap.exec> =
  object
  end

let powerbox : <Cap.all> =
  object
    (* fs *)
    method readdir = ()
    method tmp = ()
  end
