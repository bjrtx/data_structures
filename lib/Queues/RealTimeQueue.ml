type 'a t = { left : 'a Stream.t; right : 'a list; suffix : 'a Stream.t }

let empty = Stream.{ left = empty; right = []; suffix = empty }
let is_empty { left; _ } = Stream.is_empty left

let rec rotate f r a =
  let open Stream in
  lazy
    (match (f, r) with
    | (lazy Nil), y :: _ -> Cons (y, a)
    | (lazy (Cons (hf, tf))), hr :: tr ->
        Cons (hf, rotate tf tr (lazy (Cons (hr, a))))
    | _ -> failwith "Invariant failed")

let queue = function
  | { suffix = (lazy (Cons (_, tl))); _ } as q -> { q with suffix = tl }
  | { suffix = (lazy Nil); left; right } ->
      let left = rotate left right Stream.empty in
      { left; right = []; suffix = left }

let push x q = queue { q with right = x :: q.right }

let map f q = {left = Stream.map f q.left; right = List.map f q.right; suffix = Stream.map f q.suffix }

let peek = function
  | { left = (lazy Nil); _ } -> None
  | { left = (lazy (Cons (hd, _))); _ } -> Some hd

let pop = function
  | { left = (lazy Nil); _ } -> None
  | { left = (lazy (Cons (_, tl))); _ } as q -> Some { q with left = tl }
