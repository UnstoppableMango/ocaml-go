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

let handle_err { err; file; _ } offset msg =
  match err with Some err -> err (File.position file offset) msg | _ -> ()

let error s offset msg =
  let () = handle_err s offset msg in
  { s with errorCount = s.errorCount + 1 }

let decode_rune src =
  let c = Bytes.get_utf_8_uchar src 0 in
  (Uchar.utf_decode_uchar c, Uchar.utf_decode_length c)

let next s =
  let add_line s =
    let file = File.add_line s.file s.offset in
    { s with file; lineOffset = s.offset }
  in
  let offset = s.rdOffset in
  let s =
    let s = { s with offset } in
    if s.ch = '\n' then add_line s else s
  in
  if offset < Bytes.length s.src then
    match Bytes.get s.src offset with
    | '\x00' -> error s (Pos s.offset) "illegal character NUL"
    | r when r >= '\x80' ->
        (* TODO: Not ASCII *)
        (* let r, w = Bytes.sub s.src rdo (l - 1) |> decode_rune in *)
        s
    | r -> { s with rdOffset = offset + 1; ch = r }
  else { s with ch = '\x00' }

let init s file src mode =
  {
    s with
    file;
    dir = dirname file.name;
    src;
    mode;
    ch = ' ';
    offset = 0;
    rdOffset = 0;
    lineOffset = 0;
    insertSemi = false;
    errorCount = 0;
  }

let default file src = init empty file src scan_comments

let string { dir; file = { name; _ }; ch; _ } =
  Printf.sprintf "%s:%s:%c" dir name ch

type error = { pos : position; msg : string }

module Error : sig
  val string : error -> string
end = struct
  let string { pos; msg } =
    if pos.filename <> "" || is_valid pos then
      let s = Position.string pos in
      Printf.sprintf "%s: %s" s msg
    else msg
end

type error_list = error list

module ErrorList : sig end = struct end

let skip_whitespace s =
  match s.ch with
  | ' ' | '\t' | '\r' -> next s
  | '\n' when not s.insertSemi -> next s
  | _ -> s

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | ch -> ch >= '\x80' (* TODO: Unicode is_letter *)

let is_decimal = function '0' .. '9' -> true | _ -> false

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let is_digit ch = is_decimal ch (* TODO: Unicode.is_digit *)

let scan_identifier s =
  let offs = s.offset in
  String.iteri (fun i ch -> ())

let scan s =
  if Pos.is_valid s.nlPos then
    ({ s with nlPos = no_pos }, s.nlPos, Token.semicolon, "\n")
  else
    let s = skip_whitespace s in
    let pos = File.pos s.file s.offset in
    let insert_semi = false in
    (s, no_pos, Token.illegal, "")
