module Make (Const : sig
  val c : int
end) =
struct
  let c = Const.c

  open SizedStream

  type 'a t = { f : 'a SizedStream.t; r : 'a SizedStream.t }

  let empty = { f = empty; r = empty }
  let is_empty { f; r } = is_empty f && is_empty r

  let queue { f; r } =
    let lenf = size f and lenr = size r in
    if lenf > (c * lenr) + 1 then
      let i = (lenf + lenr) / 2 in
      { f = take i f; r = r @ reverse (drop i f) }
    else if lenr > (c * lenf) + 1 then
      let i = (lenf + lenr) / 2 in
      let lenr = lenf + lenr - i in
      { f = f @ reverse (drop lenr r); r = take lenr r }
    else { f; r }

  let cons x { f; r } = { f = cons x f; r } |> queue
  let head { f; r } = match peek f with None -> peek r | hd -> hd

  let tail { f; r } =
    match tail f with
    | None -> if SizedStream.is_empty r then None else Some empty
    | Some tl -> Some { f = tl; r }

  let snoc x { f; r } = { f; r = SizedStream.cons x r } |> queue

  let last { r; f } =
    match SizedStream.peek r with None -> SizedStream.peek f | bk -> bk

  let init { f; r } =
    match SizedStream.tail r with
    | None -> if SizedStream.is_empty f then None else Some empty
    | Some tl -> Some { f; r = tl }
end
