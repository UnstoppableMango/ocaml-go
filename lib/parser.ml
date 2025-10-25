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
  top : bool;
  go_version : string;
  nest_lev : int;
}

let next p = p

let init p file src mode =
  let scanner = init p.scanner file src scan_comments in
  next { p with file; mode; scanner; top = true }
