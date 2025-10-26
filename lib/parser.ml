open Ast
open Position
open Scanner
open Token

type parser = {
  file : file;
  errors : error_list;
  scanner : scanner;
  mode : mode;
  trace : bool;
  indent : int;
  comments : comment_group list;
  lead_comment : comment_group option;
  line_comment : comment_group option;
  top : bool;
  go_version : string;
  pos : pos;
  tok : token;
  lit : string;
  sync_pos : pos;
  sync_cnt : int;
  expr_lev : int;
  in_rhs : bool;
  imports : import_spec list;
  nest_lev : int;
}

module Trace : sig
  val print : parser -> unit
end = struct
  open Printf
  open String

  let dots = ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . "
  let n = length dots

  let print_dots i =
    while !i > n do
      print_string dots;
      i := !i - n
    done

  let print { pos; file; indent; _ } =
    let { line; column; _ } = File.position file pos in
    let () = printf "%5d:%3d" line column in
    let i = ref (2 * indent) in
    let () = print_dots i in

    sub dots 0 n |> print_string
end

let print_trace = Trace.print
let max_nest_lev = 100_000

let next0 p =
  let () = if p.trace && Pos.is_valid p.pos then print_trace p in
  p

let next p =
  let prev = p.pos in
  { p with lead_comment = None; line_comment = None }

let init p file src mode =
  let scanner = init p.scanner file src scan_comments in
  next { p with file; mode; scanner; top = true }
