module MakeBase (Ord : Heap.OrderedType) = struct
  type elt = Ord.t
  type t = (int * elt) Tree.Binary.t (* the first parameter is the svalue *)

  open Tree.Binary

  let empty = Tree.Binary.Empty
  let map f = Tree.Binary.map (fun (svalue, x) -> (svalue, f x))
  let size = Tree.Binary.size
  let svalue = function Empty -> -1 | Node ((s, _), _, _) -> s

  let rec merge a b =
    match (a, b) with
    | Empty, x | x, Empty -> x
    | Node ((_, va), _, _), Node ((_, vb), _, _) when Ord.compare va vb > 0 ->
        merge b a
    | Node ((_, x), l, r), b ->
        let r = merge r b in
        (* of l and r, the subtree with the larger s-value goes to the left *)
        let l, r = if svalue l >= svalue r then (l, r) else (r, l) in
        Node ((1 + min (svalue l) (svalue r), x), l, r)

  let leaf elt = Tree.Binary.leaf (0, elt)
  let push elt = merge (leaf elt)
  let step = Tree.Binary.node_func (fun (_, x) l r -> (x, merge l r))
  let peek t = Tree.Binary.node_func (fun (_, x) _ _ -> x) t

  let to_arbitrary_seq tree =
    tree |> Tree.Binary.to_arbitrary_seq |> Seq.map snd

  let rec merge_in_pairs = function
    | a :: b :: tl -> merge (merge a b) (merge_in_pairs tl)
    | [ hd ] -> hd
    | [] -> Empty

  let of_list l = l |> List.map leaf |> merge_in_pairs

  (* Todo: Leftist invariant *)
end

module Make (Ord : Heap.OrderedType) = Heap.AddOps (MakeBase (Ord))
