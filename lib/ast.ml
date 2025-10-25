open Token
open Position

type comment = { slash : pos; text : string }

module Comment : sig
  val pos : comment -> pos
  val en : comment -> pos
end = struct
  let pos { slash; _ } = slash
  let en { slash; text } = Pos (slash + String.length text)
end
