module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module Make (Ord : OrderedType) = struct
  type elt = Ord.t
  type t = {size: int; segments: elt list list Lazy.t}
  let less x y = Ord.compare x y < 0
  let rec merge xs ys = match (xs, ys) with
      |[], ys -> ys
      |xs, [] -> xs
      |(x::xs as first), (y::ys as snd) -> if less x y then x:: (merge xs snd) else y::(merge first ys)

  let new_ = {size = 0; segments = lazy []}
  let add x {size; segments} =
    let rec add_seg(seg, segs, size) =
      if size mod 2 = 0 then seg::segs else add_seg(merge seg (List.hd segs), List.tl segs, size / 2)
    in {size = succ size; segments = lazy (add_seg([x], Lazy.force segments, size))}

  let sort {segments; _} =
    let rec merge_all = function
      | xs, [] -> xs
      | xs, seg::segs -> merge_all(merge xs seg, segs)
    in merge_all ([], Lazy.force segments)
     
end
