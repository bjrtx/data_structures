module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module Make (Ord : OrderedType) = struct
  module Stream = Streams.Stream
  type elt = Ord.t
  type schedule = elt Stream.t list
  type t = { size : int; segments : (elt Stream.t * schedule) list }

  let rec merge xs ys =
    Stream.(
      match (xs, ys) with
      | (lazy Nil), ys -> ys
      | xs, (lazy Nil) -> xs
      | ((lazy (Cons (x, xs))) as first), ((lazy (Cons (y, ys))) as snd) ->
          if Ord.compare x y <= 0 then cons x (merge xs snd)
          else cons y (merge first ys))

  let rec exec1 =
    Stream.(
      function
      | [] -> []
      | (lazy Nil) :: tl -> exec1 tl
      | (lazy (Cons (_, xs))) :: tl -> xs :: tl)

  let exec2_per_seg = List.map (fun (xs, sched) -> (xs, exec1 (exec1 sched)))
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
        exec2_per_seg (add_seg (Stream.cons x (lazy Nil), segments, size, []));
    }

  let sort { segments; _ } =
    List.fold_left (fun acc (x, _) -> merge acc x) Stream.empty segments
    |> Stream.to_list
end

module type Sortable = sig
  type elt
  type t

  val empty : t
  val add : elt -> t -> t
  val sort : t -> elt list
end
