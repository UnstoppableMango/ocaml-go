open Go
open Position
open In_channel

let () =
  let path =
    match Sys.argv with
    | [| _; x |] when String.ends_with ~suffix:".go" x -> x
    | _ -> exit 1
  in
  let prog = open_in path |> input_all |> Bytes.of_string in
  let file = { File.zero with name = path } in
  let s = Scanner.default file prog in
  Scanner.string s |> print_endline
