type position = { filename : string; offset : int; line : int; column : int }

let is_valid p = p.line > 0

let string p =
  let s = p.filename in
  if is_valid p then if s <> "" then s + ":" end
