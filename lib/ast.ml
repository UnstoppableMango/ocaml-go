open Position
open Token

type comment = { slash : pos; text : string }

module Comment : sig
  val pos : comment -> pos
  val fin : comment -> pos
end = struct
  let pos { slash; _ } = slash
  let fin { slash; text } = Pos.add slash (String.length text)
end

type comment_group = { list : comment list }

module CommentGroup : sig
  val pos : comment_group -> pos
  val fin : comment_group -> pos
end = struct
  let pos { list } = list |> List.hd |> Comment.pos
  let fin { list } = list |> List.rev |> List.hd |> Comment.fin
end

type ident = { name_pos : pos; name : string }

module Ident = struct
  let pos { name_pos; _ } = name_pos
end

type basic_lit = { value_pos : pos; kind : token; value : string }

module BasicLit = struct
  let pos { value_pos; _ } = value_pos
  let fin { value_pos; value; _ } = Pos.add value_pos (String.length value)
end

type import_spec = {
  doc : comment_group option;
  name : ident option;
  path : basic_lit option;
  comment : comment_group option;
  end_pos : pos;
}

module ImportSpec : sig
  val pos : import_spec -> pos
  val fin : import_spec -> pos
end = struct
  let pos = function
    | { name = Some name; _ } -> Ident.pos name
    | { path = Some path; _ } -> BasicLit.pos path
    | _ -> Pos 0

  let fin = function
    | { end_pos = Pos 0; path = Some path; _ } -> BasicLit.fin path
    | { end_pos; _ } -> end_pos
end
