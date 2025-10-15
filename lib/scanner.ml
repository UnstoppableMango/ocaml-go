type mode = int

let scan_comments : mode = 1
let dont_insert_semis : mode = 2

type scanner = { file : Token.file; dir : string; src : string; mode : mode }

type state = {
  ch : char;
  offset : int;
  rd_offset : int;
  line_offset : int;
  insert_semi : bool;
  nl_pos : Token.pos;
}

open Angstrom

let whitespace = char ' ' <|> char '\t' <|> char '\n' <|> char '\r'

let identifier =
  take_while1 (function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
    | _ -> false)
