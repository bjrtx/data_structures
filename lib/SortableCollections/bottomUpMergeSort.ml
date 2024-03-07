module Make (Ord : OrderedType.S) = struct
  type elt = Ord.t
  type t = { size : int; segments : elt list list Lazy.t }

  let merge = List.merge Ord.compare
  let empty = { size = 0; segments = lazy [] }

  let add x { size; segments = (lazy s) } =
    let rec add_seg (seg, segs, size) =
      if size mod 2 = 0 then seg :: segs
      else add_seg (merge seg (List.hd segs), List.tl segs, size / 2)
    in
    { size = succ size; segments = lazy (add_seg ([ x ], s, size)) }

  let sort { segments = (lazy s); _ } = List.fold_left merge [] s
end
