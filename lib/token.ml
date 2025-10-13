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
  | String (* Literal end *)
  (* Operator begin *)
  | Add
  | Sub
  | Mul
  | Quo
  | Rem
  (* Keyword begin *)
  | Break
  | Case
  | Chan
  | Const
  | Continue
(* Keyword end *)

(* Operator end *)
(* TODO *)

(* TODO *)
let tokens = function
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
  | Continue -> "continue"

module P = struct
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
end
