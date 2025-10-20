open Token

type mode = Mode of int

let scan_comments = Mode 1
let dont_insert_semis = Mode 2

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
  nlPos : pos;
  errorCount : int;
}

let empty =
  {
    file = { lines = []; size = 0; base = 0; name = "" };
    dir = "";
    src = Bytes.empty;
    mode = scan_comments; (* TODO: Is this correct? *)
    ch = ' ';
    offset = 0;
    rdOffset = 0;
    lineOffset = 0;
    insertSemi = false;
    nlPos = no_pos;
    errorCount = 0;
  }

let next s =
  if s.rdOffset < Bytes.length s.src then s
  else
    {
      s with
      offset = Bytes.length s.src;
      lineOffset = (if s.ch = '\n' then s.offset else s.lineOffset);
      file = (if s.ch = '\n' then File.add_line s.file s.offset else s.file);
      ch = Char.chr 0;
    }

let init s f src mode =
  {
    s with
    file = f;
    dir = Filename.dirname f.name;
    src;
    mode;
    ch = ' ';
    offset = 0;
    rdOffset = 0;
    lineOffset = 0;
    insertSemi = false;
    errorCount = 0;
  }

(* Meh *)

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
