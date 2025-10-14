type token =
  | Illegal
  | Eof
  | Comment
  (* Literal begin *)
  | Ident
  | Int
  | Float
  | Imaginary
  | Char
  | String
  (* Literal end *)
  (* Operator begin *)
  | Add
  | Sub
  | Mul
  | Quo
  | Rem
  (* Operator end *)
  (* Keyword begin *)
  | Break
  | Case
  | Chan
  | Const
  | Continue
(* Keyword end *)
(* TODO *)

module Tokens : sig
  type t = token

  val compare : token -> token -> int
end = struct
  type t = token

  let compare = compare
end

module TokensMap = Map.Make (Tokens)

(* Somebody un-stupid this for me *)

let tokens = TokensMap.of_seq @@ List.to_seq [
    (Illegal, (0, "ILLEGAL"));
    (Eof, (1, "EOF"));
    (Comment, (2, "COMMENT"));
    (* Literal begin *)
    (Ident, (4, "IDENT"));
    (Int, (5, "INT"));
    (Float, (6, "FLOAT"));
    (Imaginary, (7, "IMAG"));
    (Char, (8, "CHAR"));
    (String, (9, "STRING"));
    (* Literal end *)
    (* Operator begin *)
    (Add, (12, "+"));
    (Sub, (13, "-"));
    (Mul, (14, "*"));
    (Quo, (15, "/"));
    (Rem, (16, "%"));
    (* Operator end *)
    (* Keyword begin *)
    (Break, (19, "break"));
    (Case, (20, "case"));
    (Chan, (21, "chan"));
    (Const, (22, "const"));
    (Continue, (23, "continue"));
    (* Keyword end *)
    (* TODO *)
  ]

(* TODO *)
(* let tokens = function
  | Illegal -> "ILLEGAL"
  | Eof -> "EOF"
  | Comment -> "COMMENT"
  (* Literals *)
  | Ident -> "IDENT"
  | Int -> "INT"
  | Float -> "FLOAT"
  | Imaginary -> "IMAG"
  | Char -> "CHAR"
  | String -> "STRING"
  (* Operators *)
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Quo -> "/"
  | Rem -> "%"
  (* Keywords *)
  | Break -> "break"
  | Case -> "case"
  | Chan -> "chan"
  | Const -> "const"
  | Continue -> "continue" *)

module P : sig
  val is_literal : token -> bool
  val is_operator : token -> bool
  val is_keyword : token -> bool
  val is_exported : string -> bool
  (* val is_identifier : string -> bool *)
end = struct
  let is_literal = function
    | Ident | Int | Float | Imaginary | Char | String -> true
    | _ -> false

  let is_operator = function Add | Sub | Mul | Quo | Rem -> true | _ -> false

  (* TODO *)
  let is_keyword = function
    | Break | Case | Chan | Const | Continue -> true
    | _ -> false

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
