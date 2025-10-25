open Filename
open Position

type mode = Mode of int

let scan_comments = Mode 1
let dont_insert_semis = Mode 2

type error_handler = position -> string -> unit

type scanner = {
  file : file;
  dir : string;
  src : bytes;
  err : error_handler option;
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
    err = None;
    (* TODO: Is this correct? *)
    mode = scan_comments;
    ch = ' ';
    offset = 0;
    rdOffset = 0;
    lineOffset = 0;
    insertSemi = false;
    nlPos = no_pos;
    errorCount = 0;
  }

let error s o msg =
  let () =
    let p = File.position s.file o in
    match s.err with Some err -> err p msg | _ -> ()
  in

  { s with errorCount = s.errorCount + 1 }

let decode_rune src =
  let c = Bytes.get_utf_8_uchar src 0 in
  (Uchar.utf_decode_uchar c, Uchar.utf_decode_length c)

let next s =
  let s =
    if s.ch == '\n' then
      let file = File.add_line s.file s.offset in
      { s with lineOffset = s.offset; file }
    else s
  in
  let l = Bytes.length s.src in
  if s.rdOffset < l then
    match Bytes.get s.src s.rdOffset with
    | '\x00' -> error s (Pos s.offset) "illegal character NUL"
    | r when r >= '\x80' ->
        let r, w = Bytes.sub s.src s.rdOffset (l - 1) |> decode_rune in
        { s with offset = s.rdOffset }
    | _ -> { s with offset = s.rdOffset }
  else { s with offset = l; ch = '\x00' }

let init s f src mode =
  {
    s with
    file = f;
    dir = dirname f.name;
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
  nl_pos : pos;
}

open Angstrom

let whitespace = char ' ' <|> char '\t' <|> char '\n' <|> char '\r'

let identifier =
  take_while1 (function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
    | _ -> false)
