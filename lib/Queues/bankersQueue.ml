module SizedStream = Streams.SizedStream

type 'a t = { left : 'a SizedStream.t; right : 'a SizedStream.t }
(* Representation invariant: leftLength is the length of left (resp. rightLength, right) and leftLength >= rightLength *)

let empty = { left = SizedStream.empty; right = SizedStream.empty }
let is_empty { left; _ } = SizedStream.is_empty left

let queue ({ left; right } as q) =
  SizedStream.(
    if size right <= size left then q
    else { left = left @ reverse right; right = empty })

let snoc x { left; right } = queue { left; right = SizedStream.cons x right }
let head { left; _ } = SizedStream.peek left

let pop { left; right } =
  SizedStream.tail left |> Option.map (fun tl -> { left = tl; right })

let map f { left; right } =
  SizedStream.{ left = map f left; right = map f right }

let size { left; right } = SizedStream.(size left + size right)
