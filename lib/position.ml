open Printf

type position = { filename : string; offset : int; line : int; column : int }

let zero = { filename = ""; offset = 0; line = 0; column = 0 }
let is_valid p = p.line > 0

let string p =
  let l = string_of_int p.line in
  match (p.filename, p.column) with
  | s, c when is_valid p -> sprintf "%s:%s:%d" s l c
  | s, 0 when is_valid p -> sprintf "%s:%s" s l
  | "", c when is_valid p -> sprintf "%s:%d" l c
  | "", 0 when is_valid p -> l
  | "", _ -> "-"
  | s, _ -> s

type pos = Pos of int

let no_pos = Pos 0

module Pos : sig
  val is_valid : pos -> bool
end = struct
  let is_valid = ( <> ) no_pos
end

(* type lineinfo = { offset : int; filename : string; line : int; column : int } *)
type file = { name : string; base : int; size : int; lines : int list }

module File : sig
  val add_line : file -> int -> file
  val base : file -> int
  val line_count : file -> int
  val name : file -> string
  val offset : file -> int -> int
  val pos : file -> int -> int
  val position : file -> pos -> position
  val position_for : file -> pos -> bool -> position
  val size : file -> int
  val zero : file
end = struct
  let name f = f.name
  let base f = f.base
  let size f = f.size
  let line_count f = f.lines |> List.length

  let add_line f o =
    let i = f |> line_count in
    if (i = 0 || o > List.nth f.lines i - 1) && o < f.size then
      { f with lines = o :: f.lines }
    else f

  let fix_offset f o =
    if o < 0 then 0 else match f.size with s when o > s -> s | _ -> o

  let pos f o = fix_offset f o + 1
  let offset f p = fix_offset f (p - f.base)
  let unpack f (_ : int) (_ : bool) = (f.name, 0, 0 (* TODO *))

  let position_for f (Pos p) a =
    if Pos p = no_pos then zero
    else
      let offset = fix_offset f (p - f.base) in
      let filename, line, column = unpack f offset a in
      { filename; offset; line; column }

  let position f p = position_for f p true
  let zero = { name = ""; base = 0; size = 0; lines = [] }
end
