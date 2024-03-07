module Make (Const : sig
  val c : int
end) =
struct
  let c = Const.c

  module SStream = Streams.SizedStream
  open SStream

  type 'a t = { f : 'a SStream.t; r : 'a SStream.t }

  let empty = { f = empty; r = empty }
  let is_empty { f; r } = is_empty f && is_empty r

  let queue ({ f; r } as q) =
    let lenf = size f and lenr = size r in
    if lenf > (c * lenr) + 1 then
      let i = (lenf + lenr) / 2 in
      { f = take i f; r = r @ reverse (drop i f) }
    else if lenr > (c * lenf) + 1 then
      let i = (lenf + lenr + 1) / 2 in
      { f = f @ reverse (drop i r); r = take i r }
    else q

  let cons x { f; r } = { f = cons x f; r } |> queue
  let head { f; r } = match peek f with None -> peek r | hd -> hd

  let tail { f; r } =
    match tail f with
    | None -> if SStream.is_empty r then None else Some empty
    | Some tl -> Some { f = tl; r }

  let snoc x { f; r } = { f; r = SStream.cons x r } |> queue

  let last { r; f } =
    match SStream.peek r with None -> SStream.peek f | bk -> bk

  let init { f; r } =
    match SStream.tail r with
    | None -> if SStream.is_empty f then None else Some empty
    | Some tl -> Some { f; r = tl }

  let to_seq { f; r } = Seq.(append (to_seq f) (to_seq @@ SStream.reverse r))
  let size { f; r } = size f + size r
end
