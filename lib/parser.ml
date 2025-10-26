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

let next p =
  let prev = p.pos in
  { p with lead_comment = None; line_comment = None }

let init p file src mode =
  let scanner = init p.scanner file src scan_comments in
  next { p with file; mode; scanner; top = true }
