open Token

type mode = int

let scan_comments : mode = 1
let dont_insert_semis : mode = 2

type scanner = {
  file : file;
  dir : string;
  src : bytes;
  mode : mode;
  ch : char;
  offset : int;
  rdOffset : int;
  lineOffset : int;
  insertSemi : bool;
  nlPos : int;
  errorCount : int;
}

let next p = p

let init f src mode =
  {
    file = f;
    dir = Filename.dirname f.name;
    src;
    mode;
    ch = ' ';
    offset = 0;
    rdOffset = 0;
    lineOffset = 0;
    insertSemi = false;
    nlPos = 0;
    errorCount = 0;
  }

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
