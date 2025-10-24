type token = Token of int

let illegal = Token 0
let eof = Token 1
let comment = Token 2
let literal_begin = Token 3
let ident = Token 4
let int = Token 5
let float = Token 6
let imag = Token 7
let char = Token 8
let string = Token 9
let literal_end = Token 10
let operator_begin = Token 11
let add = Token 12
let sub = Token 13
let mul = Token 14
let quo = Token 15
let rem = Token 16
let operator_end = Token 17
let keyword_begin = Token 18
let break = Token 19
let case = Token 20
let chan = Token 21
let const = Token 22
let continue = Token 23
let keyword_end = Token 24
(* TODO *)

module Tokens : sig
  type t = token

  val compare : token -> token -> int
end = struct
  type t = token

  let compare = compare
end

(* Somebody help me un-stupid this *)

module TokenMap = Map.Make (Tokens)

let tokens =
  TokenMap.of_list
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

let name t = tokens |> TokenMap.find_opt t |> Option.value ~default:"ILLEGAL"

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
