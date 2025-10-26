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
let aand = Token 17
let oor = Token 18
let xor = Token 19
let shl = Token 20
let shr = Token 21
let and_not = Token 22
let add_assign = Token 23
let sub_assign = Token 24
let mul_assign = Token 25
let quo_assign = Token 26
let rem_assign = Token 27
let and_assign = Token 28
let or_assign = Token 29
let xor_assign = Token 30
let shl_assign = Token 31
let shr_assign = Token 32
let and_not_assign = Token 33
let lland = Token 34
let llor = Token 35
let arrow = Token 36
let inc = Token 37
let dec = Token 38
let eql = Token 39
let lss = Token 40
let gtr = Token 41
let assign = Token 42
let not = Token 43
let neq = Token 44
let leq = Token 45
let geq = Token 46
let define = Token 47
let ellipsis = Token 48
let lparen = Token 49
let lbrack = Token 50
let lbrace = Token 51
let comma = Token 52
let period = Token 53
let rparen = Token 54
let rbrack = Token 55
let rbrace = Token 56
let semicolon = Token 57
let colon = Token 58
let operator_end = Token 59
let keyword_begin = Token 60
let break = Token 61
let case = Token 62
let chan = Token 63
let const = Token 64
let continue = Token 65
let default = Token 66
let defer = Token 67
let eelse = Token 68
let fallthrough = Token 69
let ffor = Token 70
let func = Token 71
let go = Token 72
let goto = Token 73
let iif = Token 74
let import = Token 75
let interface = Token 76
let map = Token 77
let package = Token 78
let range = Token 79
let return = Token 80
let select = Token 81
let sstruct = Token 82
let switch = Token 83
let ttype = Token 84
let var = Token 85
let keyword_end = Token 86
let additional_beg = Token 87
let tilde = Token 88
let additional_end = Token 89

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
