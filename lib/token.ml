type pos = int

let no_pos : pos = 0

module Pos : sig
  val is_valid : pos -> bool
end = struct
  let is_valid = ( <> ) no_pos
end

type token = int

let illegal : token = 0
let eof : token = 1
let comment : token = 2
let literal_begin : token = 3
let ident : token = 4
let int : token = 5
let float : token = 6
let imag : token = 7
let char : token = 8
let string : token = 9
let literal_end : token = 10
let operator_begin : token = 11
let add : token = 12
let sub : token = 13
let mul : token = 14
let quo : token = 15
let rem : token = 16
let operator_end : token = 17
let keyword_begin : token = 18
let break : token = 19
let case : token = 20
let chan : token = 21
let const : token = 22
let continue : token = 23
let keyword_end : token = 24
(* TODO *)

module Tokens : sig
  type t = token

  val compare : token -> token -> int
end = struct
  type t = token

  let compare = compare
end

(* Somebody help me un-stupid this *)

module IntMap = Map.Make (Int)

let tokens =
  IntMap.of_list
    [
      (illegal, "ILLEGAL");
      (eof, "EOF");
      (comment, "COMMENT");
      (ident, "IDENT");
      (int, "INT");
      (float, "FLOAT");
      (imag, "IMAG");
      (char, "CHAR");
      (string, "STRING");
      (add, "+");
      (sub, "-");
      (mul, "*");
      (quo, "/");
      (rem, "%");
      (break, "break");
      (case, "case");
      (chan, "chan");
      (const, "const");
      (continue, "continue");
    ]

let name t = tokens |> IntMap.find_opt t |> Option.value ~default:"ILLEGAL"

module P : sig
  val is_literal : token -> bool
  val is_operator : token -> bool
  val is_keyword : token -> bool
  val is_exported : string -> bool
  (* val is_identifier : string -> bool *)
end = struct
  let is_literal t = literal_begin < t && t < literal_end
  let is_operator t = operator_begin < t && t < operator_end
  let is_keyword t = keyword_begin < t && t < keyword_end

  (* TODO: 5.4 Ascii.is_upper *)
  let is_upper = function 'A' .. 'Z' -> true | _ -> false
  let is_exported s = String.length s > 0 && is_upper s.[0]

  (* let is_identifier s =
    if s = "" || is_keyword s then false
    else let c in String.iter s *)
end

type position = { filename : string; offset : int; line : int; column : int }

module Position : sig
  val is_valid : position -> bool
  val string : position -> string
end = struct
  let is_valid p = p.line > 0
  let string p = if is_valid p then "TODO" else "TODO"
end

type lineinfo = { offset : int; filename : string; line : int; column : int }
type file = { name : string; base : int; size : int; lines : int list }

module File : sig
  val name : file -> string
  val base : file -> int
  val size : file -> int
  val line_count : file -> int
  val add_line : file -> int -> file
  val pos : file -> int -> int
  val offset : file -> int -> int
end = struct
  let name f = f.name
  let base f = f.base
  let size f = f.size
  let line_count f = f.lines |> List.length

  let add_line f o =
    let i = f |> line_count in
    let ix = List.nth f.lines i - 1 in
    if (i = 0 || ix < o) && o < f.size then { f with lines = o :: f.lines }
    else f

  let fix_offset f o = if o < 0 then 0 else if o > f.size then f.size else o
  let pos f o = fix_offset f o + 1
  let offset f p = fix_offset f (p - f.base)
end
