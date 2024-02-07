module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module Make (Ord : OrderedType) = struct
  type elt = Ord.t
  type schedule = elt Stream.t list
  type t = { size : int; segments : (elt Stream.t * schedule) list }

  let less x y = Ord.compare x y < 0

  let rec merge xs ys =
    Stream.(
      match (xs, ys) with
      | (lazy Nil), ys -> ys
      | xs, (lazy Nil) -> xs
      | ((lazy (Cons (x, xs))) as first), ((lazy (Cons (y, ys))) as snd) ->
          if less x y then lazy (Cons (x, merge xs snd))
          else lazy (Cons (y, merge first ys)))

  let rec exec1 =
    Stream.(
      function
      | [] -> []
      | (lazy Nil) :: tl -> exec1 tl
      | (lazy (Cons (_, xs))) :: tl -> xs :: tl)

  let rec exec2_per_seg = function
    | [] -> []
    | (xs, sched) :: segs -> (xs, exec1 (exec1 sched)) :: exec2_per_seg segs

  let empty = { size = 0; segments = [] }

  let add x { size; segments } =
    let rec add_seg (seg, segs, size, rsched) =
      if size mod 2 = 0 then (seg, List.rev (seg :: rsched)) :: segs
      else
        match segs with
        | [] -> failwith "Size info is wrong"
        | (xs', _) :: segs' ->
            add_seg (merge seg xs', segs', size / 2, seg :: rsched)
    in
    {
      size = succ size;
      segments =
        exec2_per_seg (add_seg (lazy (Cons (x, lazy Nil)), segments, size, []));
    }

  let sort { segments; _ } =
    let rec merge_all = function
      | xs, [] -> xs
      | xs, (xs', _) :: segs -> merge_all (merge xs xs', segs)
    in
    merge_all (lazy Nil, segments) |> Stream.to_list
end

module type Sortable = sig
  type elt
  type t

  val empty : t
  val add : elt -> t -> t
  val sort : t -> elt list
end
