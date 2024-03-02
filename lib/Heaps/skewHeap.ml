module MakeBase (Ord : Heap.OrderedType) = struct
  open Tree.Binary

  type elt = Ord.t
  type t = elt Tree.Binary.t

  let size = Tree.Binary.size
  let empty = Tree.Binary.empty
  let to_arbitrary_seq = Tree.Binary.to_arbitrary_seq
  let map = Tree.Binary.map

  let rec merge a b =
    match (a, b) with
    | Empty, x | x, Empty -> x
    | Node (va, la, ra), Node (vb, lb, rb) ->
        if Ord.compare va vb <= 0 then Node (va, merge ra b, la)
        else Node (vb, merge rb a, lb)

  let push elt = merge (Tree.Binary.leaf elt)
  let step heap = Tree.Binary.node_func (fun v l r -> (v, merge l r)) heap
  let peek heap = Tree.Binary.node_func (fun v _ _ -> v) heap

  let rec merge_in_pairs = function
    | a :: b :: tl -> merge (merge a b) (merge_in_pairs tl)
    | [ hd ] -> hd
    | [] -> Empty

  let of_list l = l |> List.map Tree.Binary.leaf |> merge_in_pairs
end

module Make (Ord : Heap.OrderedType) = Heap.AddOps (MakeBase (Ord))
